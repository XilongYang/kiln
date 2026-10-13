module Kiln.Build.Post (renderPosts, postSlugs) where

import Control.Monad (filterM, when)
import Data.Aeson (encodeFile)
import Data.List (nub, sort, (\\))
import Kiln.Build.Cache (cacheFile, componentsFingerprint, isFileFresh, isGlobalFresh, recordFile, recordGlobal)
import Kiln.Build.Env
  ( BuildEnv (..)
  , cachePath
  , postOutputPath
  , postWorkDir
  , logMsg
  , warn
  , withContext
  , workPath
  )
import Kiln.Build.Markdown (olify, rewriteLanguageMarks, splitAbstract)
import Kiln.Build.PostEntry (PostEntry (..), PostFrontmatter (..), frontmatterTemplate, loadFrontmatter, loadPostEntry)
import Kiln.Config (PostPageConfig (..), TocConfig (..))
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | Every filesystem path touched while rendering one post, besides its
-- own standalone page (optional -- see `Kiln.Config.PostPageConfig` --
-- so handled separately rather than kept here). Grouping these avoids
-- passing a run of same-typed `FilePath` arguments positionally, where
-- it's easy to pass them in the wrong order.
data PostPaths = PostPaths
  { postSrcPath             :: FilePath -- ^ the post's original markdown
  , postRewrittenPath       :: FilePath -- ^ language-mark-rewritten, abstract-split body fed to pandoc
  , postAbstractSrcPath     :: FilePath -- ^ the abstract's markdown, split off the body
  , postAbstractHtmlPath    :: FilePath -- ^ the rendered abstract HTML
  , postFrontmatterTemplate :: FilePath -- ^ the frontmatter template, @frontmatter.tpl@
  , postFrontmatterPath     :: FilePath -- ^ the rendered frontmatter fields
  , postContentHtmlPath     :: FilePath -- ^ the rendered body HTML, for other pages' @posts@ metadata
  , postSearchTextPath      :: FilePath -- ^ the rendered plain text, for the search database
  , postTocTemplate         :: FilePath -- ^ the bare @$toc$@ template, @toc.tpl@
  , postTocHtmlPath         :: FilePath -- ^ the rendered standalone toc fragment
  , postStageCachePath      :: FilePath -- ^ cached source mtime/size/hash, under @\<cache\>/stage@
  , postItemCachePath       :: FilePath -- ^ cached `PostEntry` json, under @\<cache\>/items@
  , postSearchCachePath     :: FilePath -- ^ cached search text, under @\<cache\>/search@
  }

-- | The slug of every post kiln knows about: those with a source file in
-- `envSrcDir` right now, plus any it has rendered before and still holds
-- a cache entry for.
--
-- This -- kiln's own record of what it built -- is the authority on which
-- files in a post output directory are kiln's, which both `sweepOrphans`
-- and @kiln clean@ need. Listing the output directory instead would take
-- every @.html@ in it for a post page, which is wrong as soon as the
-- directory holds anything else (a site putting its posts at the root
-- alongside @index.html@, say).
postSlugs :: FilePath -> FilePath -> IO [String]
postSlugs srcDir cacheDir = do
  current <- sourceSlugs srcDir
  cached <- cachedSlugs cacheDir
  pure (sort (nub (current ++ cached)))

sourceSlugs :: FilePath -> IO [String]
sourceSlugs srcDir = do
  exists <- doesDirectoryExist srcDir
  if not exists then pure [] else map takeBaseName <$> sourceNames srcDir

cachedSlugs :: FilePath -> IO [String]
cachedSlugs cacheDir = do
  exists <- doesDirectoryExist stageDir
  if not exists
    then pure []
    else map takeBaseName . filter ((== ".src") . takeExtension) <$> listDirectory stageDir
  where
    stageDir = cacheDir </> "stage"

-- | The markdown files in `srcDir`, sorted.
--
-- Sorted because the order posts are rendered in reaches the output:
-- it's the order of the generated search index, and it's baked into the
-- fingerprint every page's cache is keyed on. Left in `listDirectory`'s
-- order -- which is the filesystem's, not any order a reader would
-- recognise -- the search index churned between rebuilds and pages could
-- miss their cache for no reason at all, differently on each machine.
--
-- Filtered to actual files: a *directory* named @foo.md@ passes an
-- extension test just as happily, and then reading it as a post fails
-- with an error about the directory rather than about the config.
sourceNames :: FilePath -> IO [FilePath]
sourceNames srcDir = do
  names <- listDirectory srcDir
  files <- filterM (doesFileExist . (srcDir </>)) names
  pure (sort (filter ((== ".md") . takeExtension) files))

-- | Render every markdown post in `env`'s source directory -- its own
-- standalone page too, if `env` configures one (see
-- `Kiln.Config.PostPageConfig`). Returns each post's `PostEntry` (fed
-- into every page's @posts@ metadata, see `Kiln.Build.Page`) paired with
-- its plain-text content (for the search database).
--
-- A post is skipped (and its cached outputs reused) when neither its
-- source nor anything that could affect every post's rendering --
-- `envWebroot`, the toc/prism/math settings, the post page template, or
-- kiln's own version -- has changed since the last build; see
-- `Kiln.Build.Cache`. Posts whose source has disappeared are warned
-- about but left in place, since they may still be linked from
-- elsewhere; a post's cache is only dropped once both its source and its
-- rendered output are gone.
renderPosts :: BuildEnv -> IO [(PostEntry, String)]
renderPosts env = do
  warnAboutSubdirectories (envSrcDir env)
  names <- sourceNames (envSrcDir env)
  maybe (pure ()) (createDirectoryIfMissing True . postPageOutputDir) (envPost env)
  createDirectoryIfMissing True (envWorkDir env)
  writeFile (frontmatterTemplatePath env) frontmatterTemplate
  writeFile (tocTemplatePath env) "$toc$"
  postTemplate <- maybe (pure "") (readFile' . (envTemplateDir env </>) . postPageTemplate) (envPost env)
  components <- componentsFingerprint (envTemplateDir env)
  let fingerprint =
        unlines
          [ envTools env
          , envWebroot env
          , show (envToc env)
          , show (envPrism env)
          , show (envMath env)
          , postTemplate
          , components
          ]
  globalFresh <- isGlobalFresh globalCachePath fingerprint
  results <- mapM (renderPost env globalFresh) names
  recordGlobal globalCachePath fingerprint
  sweepOrphans env (map takeBaseName names)
  pure results
  where
    globalCachePath = cachePath env ["global"]

frontmatterTemplatePath :: BuildEnv -> FilePath
frontmatterTemplatePath env = workPath env ["frontmatter.tpl"]

tocTemplatePath :: BuildEnv -> FilePath
tocTemplatePath env = workPath env ["toc.tpl"]

-- | Warn about any directory sitting in the source directory. Posts are
-- read from it non-recursively, so a directory of markdown there is
-- quietly built into nothing at all -- cheaper to say so than to leave
-- someone wondering where their posts went.
warnAboutSubdirectories :: FilePath -> IO ()
warnAboutSubdirectories srcDir = do
  exists <- doesDirectoryExist srcDir
  when exists $ do
    names <- sort <$> listDirectory srcDir
    dirs <- filterM (doesDirectoryExist . (srcDir </>)) names
    mapM_ (\d -> warn (srcDir </> d ++ " is a directory; posts are only read from " ++ srcDir ++ " itself")) dirs

-- | Warn about any post kiln has rendered whose source is no longer
-- among `currentSlugs` -- left alone, since it may still be linked from
-- elsewhere -- and silently drop the cache entry for any post that has
-- neither a source nor a rendered output left: pure bookkeeping nobody
-- can see, so there's nothing to warn about.
sweepOrphans :: BuildEnv -> [String] -> IO ()
sweepOrphans env currentSlugs = do
  known <- cachedSlugs (envCacheDir env)
  mapM_ sweepSlug (known \\ currentSlugs)
  where
    sweepSlug slug = do
      rendered <- maybe (pure False) doesFileExist (postOutputPath env slug)
      if rendered
        then warn (slug ++ ".html has no matching source file; leaving it in place")
        else
          mapM_
            removeIfExists
            [ cachePath env ["stage", slug ++ ".src"]
            , cachePath env ["items", slug ++ ".json"]
            , cachePath env ["search", slug ++ ".txt"]
            ]

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

renderPost :: BuildEnv -> Bool -> FilePath -> IO (PostEntry, String)
renderPost env globalFresh name =
  withContext ("while building " ++ (envSrcDir env </> name)) $ do
    htmlFresh <- maybe (pure True) doesFileExist (postOutputPath env slug)
    fresh <-
      if globalFresh && htmlFresh
        then isFileFresh (postStageCachePath paths) (postSrcPath paths)
        else pure False
    if fresh
      then do
        entry <- loadPostEntry (postItemCachePath paths)
        content <- readFile' (postSearchCachePath paths)
        pure (entry, content)
      else do
        -- Only the posts actually rendered are named. A post served from
        -- cache produced no work to report, and on a site of a few
        -- hundred saying so for each would bury the handful that did --
        -- which is the one thing an incremental build has to tell you.
        logMsg ("  " ++ name)
        rebuild
  where
    slug = takeBaseName name
    workDir = postWorkDir env slug
    paths =
      PostPaths
        { postSrcPath = envSrcDir env </> name
        , postRewrittenPath = workDir </> "body.md"
        , postAbstractSrcPath = workDir </> "abstract.md"
        , postAbstractHtmlPath = workDir </> "abstract.html"
        , postFrontmatterTemplate = frontmatterTemplatePath env
        , postFrontmatterPath = workDir </> "frontmatter.txt"
        , postContentHtmlPath = workDir </> "content.html"
        , postSearchTextPath = workDir </> "search.txt"
        , postTocTemplate = tocTemplatePath env
        , postTocHtmlPath = workDir </> "toc.html"
        , postStageCachePath = cachePath env ["stage", slug ++ ".src"]
        , postItemCachePath = cachePath env ["items", slug ++ ".json"]
        , postSearchCachePath = cachePath env ["search", slug ++ ".txt"]
        }
    rebuild = do
      createDirectoryIfMissing True workDir
      src <- readFile' (postSrcPath paths)
      let prepared = if envPrism env then rewriteLanguageMarks src else src
          (abstractSrc, bodySrc) = splitAbstract prepared
      writeFile (postRewrittenPath paths) bodySrc
      abstractHtml <- traverse (renderAbstractHtml paths) abstractSrc
      contentHtml <- renderContentHtml paths
      mapM_ (\ppc -> renderPostHtml env ppc slug paths abstractHtml) (envPost env)
      renderFrontmatter paths
      renderPostSearchText paths
      frontmatter <- loadFrontmatter (postFrontmatterPath paths)
      content <- readFile' (postSearchTextPath paths)
      let entry =
            PostEntry
              { postTitle = fmTitle frontmatter
              , postDate = fmDate frontmatter
              , postSlug = slug
              , postTags = fmTags frontmatter
              , postAbstract = abstractHtml
              , postContent = contentHtml
              }
      createDirectoryIfMissing True (takeDirectory (postItemCachePath paths))
      encodeFile (postItemCachePath paths) entry
      cacheFile (postSearchCachePath paths) (postSearchTextPath paths)
      recordFile (postStageCachePath paths) (postSrcPath paths)
      pure (entry, content)

-- | Run an abstract's markdown through pandoc's HTML writer (no
-- template, just the rendered fragment) so it can be spliced into the
-- post page as the @abstract@ variable.
renderAbstractHtml :: PostPaths -> String -> IO String
renderAbstractHtml paths abstractSrc = do
  writeFile (postAbstractSrcPath paths) abstractSrc
  callProcess
    "pandoc"
    [ "--quiet"
    , "--to=html"
    , "--wrap=none"
    , "--output=" ++ postAbstractHtmlPath paths
    , postAbstractSrcPath paths
    ]
  readFile' (postAbstractHtmlPath paths)

-- | Run the post's rewritten body (already written to
-- `postRewrittenPath`) through pandoc's HTML writer (no template, just
-- the rendered fragment), so other pages can include this post's full
-- content in their own @posts@ metadata without re-rendering it
-- themselves.
renderContentHtml :: PostPaths -> IO String
renderContentHtml paths = do
  callProcess
    "pandoc"
    [ "--quiet"
    , "--to=html"
    , "--wrap=none"
    , "--output=" ++ postContentHtmlPath paths
    , postRewrittenPath paths
    ]
  readFile' (postContentHtmlPath paths)

-- | Run the rewritten post body through pandoc using `ppc`'s page
-- template, writing the rendered post page. The table of contents, if
-- enabled, is rendered separately (see `renderTocHtml`) so that -- when
-- the site asks for a numbered one -- its @<ul>@s can be rewritten into
-- @<ol>@s before being spliced in as the @toc@ variable; pandoc's own
-- @--toc@ flag only ever emits @<ul>@s.
renderPostHtml :: BuildEnv -> PostPageConfig -> String -> PostPaths -> Maybe String -> IO ()
renderPostHtml env ppc slug paths abstractHtml = do
  tocFlag <-
    if tocEnable toc
      then do
        tocHtml <- renderTocHtml toc paths
        pure ["--variable=toc=" ++ (if tocOrdered toc then olify tocHtml else tocHtml)]
      else pure []
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--standalone"
      , "--template=" ++ (envTemplateDir env </> postPageTemplate ppc)
      , "--variable=webroot=" ++ envWebroot env
      , "--variable=slug=" ++ slug
      , "--output=" ++ (postPageOutputDir ppc </> slug ++ ".html")
      , postRewrittenPath paths
      ]
        ++ ["--mathjax" | envMath env]
        ++ tocFlag
        ++ abstractFlag
    )
  where
    toc = envToc env
    abstractFlag = maybe [] (\h -> ["--variable=abstract=" ++ h]) abstractHtml

-- | Render the post body's table of contents on its own, via a template
-- that's just @$toc$@, so it can be post-processed independently of the
-- post's own body (which may contain unrelated @<ul>@s of its own).
renderTocHtml :: TocConfig -> PostPaths -> IO String
renderTocHtml toc paths = do
  callProcess
    "pandoc"
    [ "--quiet"
    , "--toc"
    , "--toc-depth=" ++ show (tocDepth toc)
    , "--template=" ++ postTocTemplate paths
    , "--output=" ++ postTocHtmlPath paths
    , postRewrittenPath paths
    ]
  readFile' (postTocHtmlPath paths)

-- | Run the post's source through pandoc using `frontmatterTemplate` to
-- extract its title, date and tags.
renderFrontmatter :: PostPaths -> IO ()
renderFrontmatter paths =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--standalone"
    , "--to=plain"
    , "--wrap=none"
    , "--template=" ++ postFrontmatterTemplate paths
    , "--output=" ++ postFrontmatterPath paths
    , postSrcPath paths
    ]

-- | Run the post's source through pandoc's plain writer (no template,
-- just the body) for the search database to index.
renderPostSearchText :: PostPaths -> IO ()
renderPostSearchText paths =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--to=plain"
    , "--wrap=none"
    , "--output=" ++ postSearchTextPath paths
    , postSrcPath paths
    ]
