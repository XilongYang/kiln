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
  , mapBounded
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

-- | Every path touched while rendering one post, grouped so a run of
-- same-typed `FilePath` arguments can't be passed in the wrong order.
-- The standalone page is left out, being optional (see
-- `Kiln.Config.PostPageConfig`).
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

-- | Every slug kiln knows about: sourced in `envSrcDir` now, or rendered
-- before and still cached.
--
-- kiln's own record is the authority on which files in an output
-- directory are its, as `sweepOrphans` and @kiln clean@ both need:
-- listing the directory would take every @.html@ for a post page, wrong
-- the moment posts sit beside an @index.html@.
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

-- | The markdown files in `srcDir`.
--
-- Sorted because the render order reaches the output -- it is the search
-- index's order and part of every page's cache fingerprint -- and
-- `listDirectory`'s order is the filesystem's, so it would churn both
-- differently on each machine.
--
-- Files only: a *directory* named @foo.md@ passes the extension test and
-- then fails as an error about the directory rather than the config.
sourceNames :: FilePath -> IO [FilePath]
sourceNames srcDir = do
  names <- listDirectory srcDir
  files <- filterM (doesFileExist . (srcDir </>)) names
  pure (sort (filter ((== ".md") . takeExtension) files))

-- | Render every markdown post in `env`'s source directory, its
-- standalone page included if one is configured. Returns each post's
-- `PostEntry` (for every page's @posts@ metadata) paired with its
-- plain-text content (for the search database).
--
-- A post is served from cache when neither its source nor anything
-- affecting every post -- `envWebroot`, the toc\/prism\/math settings,
-- the post page template, kiln's version -- has changed. A post whose
-- source is gone is warned about but left in place, as it may still be
-- linked from elsewhere.
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
  results <- mapBounded (renderPost env globalFresh) names
  recordGlobal globalCachePath fingerprint
  sweepOrphans env (map takeBaseName names)
  pure results
  where
    globalCachePath = cachePath env ["global"]

frontmatterTemplatePath :: BuildEnv -> FilePath
frontmatterTemplatePath env = workPath env ["frontmatter.tpl"]

tocTemplatePath :: BuildEnv -> FilePath
tocTemplatePath env = workPath env ["toc.tpl"]

-- | Warn about a directory in the source directory: posts are read
-- non-recursively, so markdown in there silently builds into nothing.
warnAboutSubdirectories :: FilePath -> IO ()
warnAboutSubdirectories srcDir = do
  exists <- doesDirectoryExist srcDir
  when exists $ do
    names <- sort <$> listDirectory srcDir
    dirs <- filterM (doesDirectoryExist . (srcDir </>)) names
    mapM_ (\d -> warn (srcDir </> d ++ " is a directory; posts are only read from " ++ srcDir ++ " itself")) dirs

-- | Warn about a rendered post missing from `currentSlugs`, leaving it
-- in place in case it is still linked. A post with neither source nor
-- output left is dropped from the cache silently -- bookkeeping nobody
-- can see.
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
        -- Only posts actually rendered are named: on a site of a few
        -- hundred, reporting the cached ones would bury the handful an
        -- incremental build exists to tell you about.
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
      abstractHtml <- traverse (renderAbstractHtml env paths) abstractSrc
      contentHtml <- renderContentHtml env paths
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

-- | Pandoc's own highlighter, off once `Kiln.Config.optPrism` has taken
-- the job: a rewritten fence's classes name no language pandoc knows,
-- and it searches every syntax it has, per code block.
noHighlight :: BuildEnv -> [String]
noHighlight env = ["--no-highlight" | envPrism env]

-- | The abstract as an HTML fragment, for the post page's @abstract@
-- variable.
renderAbstractHtml :: BuildEnv -> PostPaths -> String -> IO String
renderAbstractHtml env paths abstractSrc = do
  writeFile (postAbstractSrcPath paths) abstractSrc
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--to=html"
      , "--wrap=none"
      , "--output=" ++ postAbstractHtmlPath paths
      , postAbstractSrcPath paths
      ]
        ++ noHighlight env
    )
  readFile' (postAbstractHtmlPath paths)

-- | The post body as an HTML fragment, so other pages can carry this
-- post's content in their @posts@ metadata without re-rendering it.
renderContentHtml :: BuildEnv -> PostPaths -> IO String
renderContentHtml env paths = do
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--to=html"
      , "--wrap=none"
      , "--output=" ++ postContentHtmlPath paths
      , postRewrittenPath paths
      ]
        ++ noHighlight env
    )
  readFile' (postContentHtmlPath paths)

-- | Write the post's standalone page through `ppc`'s template. The toc
-- is rendered separately (see `renderTocHtml`) so a numbered one can have
-- its @<ul>@s rewritten before being spliced in: pandoc's @--toc@ only
-- ever emits @<ul>@s.
renderPostHtml :: BuildEnv -> PostPageConfig -> String -> PostPaths -> Maybe String -> IO ()
renderPostHtml env ppc slug paths abstractHtml = do
  tocFlag <-
    if tocEnable toc
      then do
        tocHtml <- renderTocHtml env paths
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
        ++ noHighlight env
        ++ tocFlag
        ++ abstractFlag
    )
  where
    toc = envToc env
    abstractFlag = maybe [] (\h -> ["--variable=abstract=" ++ h]) abstractHtml

-- | The toc alone, via a template that is just @$toc$@, so rewriting it
-- can't reach the @<ul>@s in the post's own body.
renderTocHtml :: BuildEnv -> PostPaths -> IO String
renderTocHtml env paths = do
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--toc"
      , "--toc-depth=" ++ show (tocDepth (envToc env))
      , "--template=" ++ postTocTemplate paths
      , "--output=" ++ postTocHtmlPath paths
      , postRewrittenPath paths
      ]
        ++ noHighlight env
    )
  readFile' (postTocHtmlPath paths)

-- | Write the post's title, date and tags to `postFrontmatterPath`,
-- via `frontmatterTemplate`.
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

-- | Write the post as plain text to `postSearchTextPath`, for the
-- search database to index.
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
