module Kiln.Build.Post (renderPosts) where

import Control.Monad (when)
import Data.Aeson (encodeFile)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, (\\))
import Kiln.Build.Cache (cacheFile, componentsFingerprint, isFileFresh, isGlobalFresh, recordFile, recordGlobal)
import Kiln.Build.PostEntry (PostEntry (..), PostFrontmatter (..), frontmatterTemplate, loadFrontmatter, loadPostEntry)
import Kiln.Str (replaceAll, trim)
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..), PostPageConfig (..), TocConfig (..))
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, readFile', stdout)
import System.Process (callProcess)

-- | Metadata shared across a post's render steps that isn't itself a
-- filesystem path: its slug, the site's webroot, and the table-of-contents
-- settings.
data PostMeta = PostMeta
  { metaSlug    :: String
  , postWebroot :: String
  , postToc     :: TocConfig
  }

-- | Every filesystem path touched while rendering one post, besides its
-- own standalone page (optional -- see `Kiln.Config.PostPageConfig` --
-- so handled separately rather than kept here). Grouping these avoids
-- passing a run of same-typed `FilePath`/`String` arguments positionally,
-- where it's easy to pass them in the wrong order.
data PostPaths = PostPaths
  { postSrcPath             :: FilePath -- ^ the post's original markdown
  , postRewrittenPath       :: FilePath -- ^ language-mark-rewritten, abstract-split body fed to pandoc
  , postFrontmatterTemplate :: FilePath -- ^ the frontmatter JSON template, @frontmatter.json.tpl@
  , postFrontmatterJsonPath :: FilePath -- ^ the rendered frontmatter JSON (title/date)
  , postContentHtmlPath     :: FilePath -- ^ the rendered body HTML, for other pages' @posts@ metadata
  , postSearchTextPath      :: FilePath -- ^ the rendered plain text, for the search database
  , postTocTemplate         :: FilePath -- ^ the bare @$toc$@ template, @toc.tpl@
  , postTocHtmlPath         :: FilePath -- ^ the rendered standalone toc fragment
  , postStageCachePath      :: FilePath -- ^ cached source mtime/size/hash, under @.cache/stage@
  , postItemCachePath       :: FilePath -- ^ cached `PostEntry` json, under @.cache/items@
  , postSearchCachePath     :: FilePath -- ^ cached search text, under @.cache/search@
  }

-- | Render every markdown post in `config`'s source directory -- its own
-- standalone page too, if `config` configures one (see
-- `Kiln.Config.PostPageConfig`). Returns each post's `PostEntry` (fed
-- into every page's @posts@ metadata, see `Kiln.Build.Page`) paired with
-- its plain-text content (for the search database).
--
-- A post is skipped (and its cached outputs reused) when neither its
-- source nor anything that could affect every post's rendering --
-- `webroot`, the `toc` settings, or the post page template itself -- has
-- changed since the last build; see `Kiln.Build.Cache`. Posts whose
-- source has disappeared are warned about but left in place, since they
-- may still be linked from elsewhere; a post's cache is only dropped once
-- both its source and its rendered output are gone.
renderPosts :: KilnConfig -> FilePath -> IO [(PostEntry, String)]
renderPosts config tempDir = do
  names <- listDirectory srcDir
  let mdNames = filter ((== ".md") . takeExtension) names
  createDirectoryIfMissing True searchItemDir
  maybe (pure ()) (createDirectoryIfMissing True . postPageOutput) postPageCfg
  writeFile frontmatterTemplatePath frontmatterTemplate
  writeFile tocTemplatePath "$toc$"
  postTemplate <- maybe (pure "") (readFile' . (tempDir </>) . postPageTemplate) postPageCfg
  components <- componentsFingerprint tempDir
  let fingerprint = configWebroot config ++ "\n" ++ show (configToc config) ++ "\n" ++ postTemplate ++ "\n" ++ components
  globalFresh <- isGlobalFresh globalCachePath fingerprint
  results <- mapM (renderPost config tempDir globalFresh) mdNames
  recordGlobal globalCachePath fingerprint
  sweepOrphans postPageCfg outPaths (map takeBaseName mdNames)
  pure results
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    postPageCfg = configPost config
    searchItemDir = tempDir </> "search-item"
    frontmatterTemplatePath = tempDir </> "frontmatter.json.tpl"
    tocTemplatePath = tempDir </> "toc.tpl"
    globalCachePath = outCache outPaths </> "global"

-- | Warn about any rendered post whose source is no longer among
-- `currentSlugs` (left alone, since it may still be linked from
-- elsewhere) -- skipped entirely when `config` has no `PostPageConfig`,
-- since there's no rendered page to find in the first place -- and
-- silently drop the `outCache` entry for any post that has neither a
-- source nor a rendered output left -- pure bookkeeping nobody can see,
-- so there's nothing to warn about.
sweepOrphans :: Maybe PostPageConfig -> OutPaths -> [String] -> IO ()
sweepOrphans postPageCfg outPaths currentSlugs = do
  postSlugs <- case postPageCfg of
    Just ppc -> map takeBaseName . filter ((== ".html") . takeExtension) <$> listDirectory (postPageOutput ppc)
    Nothing  -> pure []
  stageExists <- doesDirectoryExist stageCacheDir
  cachedSlugs <-
    if stageExists
      then map takeBaseName . filter ((== ".src") . takeExtension) <$> listDirectory stageCacheDir
      else pure []
  let sweepSlug slug
        | slug `elem` postSlugs =
            warn (slug ++ ".html has no matching source file; leaving it in place")
        | otherwise =
            mapM_
              removeIfExists
              [ stageCacheDir </> slug ++ ".src"
              , outCache outPaths </> "items" </> slug ++ ".json"
              , outCache outPaths </> "search" </> slug ++ ".txt"
              ]
  mapM_ sweepSlug (nub (postSlugs ++ cachedSlugs) \\ currentSlugs)
  where
    stageCacheDir = outCache outPaths </> "stage"

warn :: String -> IO ()
warn = hPutStrLn stdout . ("Warning: " ++)

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

renderPost :: KilnConfig -> FilePath -> Bool -> FilePath -> IO (PostEntry, String)
renderPost config tempDir globalFresh name = do
  htmlFresh <- maybe (pure True) (doesFileExist . (</> slug ++ ".html") . postPageOutput) postPageCfg
  fresh <-
    if globalFresh && htmlFresh
      then isFileFresh (postStageCachePath paths) (postSrcPath paths)
      else pure False
  if fresh
    then do
      entry <- loadPostEntry (postItemCachePath paths)
      content <- readFile' (postSearchCachePath paths)
      pure (entry, content)
    else rebuild
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    cacheDir = outCache outPaths
    postPageCfg = configPost config
    slug = takeBaseName name
    meta =
      PostMeta
        { metaSlug = slug
        , postWebroot = configWebroot config
        , postToc = configToc config
        }
    paths =
      PostPaths
        { postSrcPath = srcDir </> name
        , postRewrittenPath = tempDir </> name
        , postFrontmatterTemplate = tempDir </> "frontmatter.json.tpl"
        , postFrontmatterJsonPath = tempDir </> "frontmatter.json"
        , postContentHtmlPath = tempDir </> slug ++ "-content.html"
        , postSearchTextPath = tempDir </> "search-item" </> slug ++ ".txt"
        , postTocTemplate = tempDir </> "toc.tpl"
        , postTocHtmlPath = tempDir </> slug ++ "-toc.html"
        , postStageCachePath = cacheDir </> "stage" </> slug ++ ".src"
        , postItemCachePath = cacheDir </> "items" </> slug ++ ".json"
        , postSearchCachePath = cacheDir </> "search" </> slug ++ ".txt"
        }
    rebuild = do
      src <- readFile' (postSrcPath paths)
      let (abstractSrc, bodySrc) = splitAbstract (rewriteLanguageMarks src)
      writeFile (postRewrittenPath paths) bodySrc
      abstractHtml <- traverse (renderAbstractHtml tempDir) abstractSrc
      contentHtml <- renderContentHtml paths
      case postPageCfg of
        Just ppc -> renderPostHtml ppc tempDir meta paths abstractHtml
        Nothing  -> pure ()
      renderFrontmatterJson paths
      renderPostSearchText paths
      frontmatter <- loadFrontmatter (postFrontmatterJsonPath paths)
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

-- | Split a post's (already language-mark-rewritten) source on a line
-- consisting solely of the @<!--more-->@ marker. The text before the
-- marker becomes the post's abstract; the text after it (reattached to
-- any YAML frontmatter, so pandoc still sees the post's title/date/etc.)
-- becomes the body that gets rendered. Posts without the marker are
-- passed through unchanged, with no abstract.
splitAbstract :: String -> (Maybe String, String)
splitAbstract src = case break isMoreMarker rest of
  (before, _ : after) -> (Just (trim (unlines before)), unlines (frontmatter ++ "" : after))
  (_, [])             -> (Nothing, src)
  where
    (frontmatter, rest) = splitFrontmatter (lines src)
    isMoreMarker = (== "<!--more-->") . trim

-- | Split off a leading YAML metadata block (the @---@-delimited lines,
-- delimiters included) from the rest of a post's lines, if one is
-- present.
splitFrontmatter :: [String] -> ([String], [String])
splitFrontmatter (l : ls)
  | trim l == "---" = case break ((== "---") . trim) ls of
      (fm, closing : rest) -> (l : fm ++ [closing], rest)
      (_, [])              -> ([], l : ls)
splitFrontmatter ls = ([], ls)

-- | Run an abstract's markdown through pandoc's HTML writer (no
-- template, just the rendered fragment) so it can be spliced into the
-- post page as the @abstract@ variable.
renderAbstractHtml :: FilePath -> String -> IO String
renderAbstractHtml tempDir abstractSrc = do
  writeFile abstractSrcPath abstractSrc
  callProcess "pandoc" ["--quiet", "--to=html", "--wrap=none", "--output=" ++ abstractHtmlPath, abstractSrcPath]
  readFile' abstractHtmlPath
  where
    abstractSrcPath = tempDir </> "abstract.md"
    abstractHtmlPath = tempDir </> "abstract.html"

-- | Run the post's rewritten body (already written to
-- `postRewrittenPath`) through pandoc's HTML writer (no template, just
-- the rendered fragment), so other pages can include this post's full
-- content in their own @posts@ metadata without re-rendering it
-- themselves.
renderContentHtml :: PostPaths -> IO String
renderContentHtml paths = do
  callProcess "pandoc" ["--quiet", "--to=html", "--wrap=none", "--output=" ++ contentHtmlPath, postRewrittenPath paths]
  readFile' contentHtmlPath
  where
    contentHtmlPath = postContentHtmlPath paths

-- | Run the rewritten post body through pandoc using `ppc`'s page
-- template, writing the rendered post page. The table of contents, if
-- enabled, is rendered separately (see `renderTocHtml`) so its @<ul>@s
-- can be rewritten into @<ol>@s before being spliced in as the @toc@
-- variable; pandoc's own `--toc` flag only ever emits @<ul>@s.
renderPostHtml :: PostPageConfig -> FilePath -> PostMeta -> PostPaths -> Maybe String -> IO ()
renderPostHtml ppc tempDir meta paths abstractHtml = do
  tocFlag <- if tocEnable toc
    then do
      tocHtml <- renderTocHtml meta paths
      pure ["--variable=toc=" ++ olify tocHtml]
    else pure []
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--standalone"
      , "--mathjax"
      , "--template=" ++ (tempDir </> postPageTemplate ppc)
      , "--variable=webroot=" ++ postWebroot meta
      , "--variable=slug=" ++ metaSlug meta
      , "--output=" ++ (postPageOutput ppc </> metaSlug meta ++ ".html")
      , postRewrittenPath paths
      ]
      ++ tocFlag
      ++ abstractFlag
    )
  where
    toc = postToc meta
    abstractFlag = maybe [] (\h -> ["--variable=abstract=" ++ h]) abstractHtml

-- | Render the post body's table of contents on its own, via a template
-- that's just @$toc$@, so it can be post-processed independently of the
-- post's own body (which may contain unrelated @<ul>@s of its own).
renderTocHtml :: PostMeta -> PostPaths -> IO String
renderTocHtml meta paths = do
  callProcess
    "pandoc"
    [ "--quiet"
    , "--toc"
    , "--toc-depth=" ++ show (tocDepth (postToc meta))
    , "--template=" ++ postTocTemplate paths
    , "--output=" ++ postTocHtmlPath paths
    , postRewrittenPath paths
    ]
  readFile' (postTocHtmlPath paths)

-- | Rewrite a rendered toc fragment's @<ul>@/@</ul>@ tags into
-- @<ol>@/@</ol>@, so the toc reads as a numbered list.
olify :: String -> String
olify = replaceAll "</ul>" "</ol>" . replaceAll "<ul>" "<ol>"

-- | Run the post's source through pandoc using `frontmatterTemplate` to
-- extract its title/date as JSON.
renderFrontmatterJson :: PostPaths -> IO ()
renderFrontmatterJson paths =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--standalone"
    , "--to=plain"
    , "--wrap=none"
    , "--template=" ++ postFrontmatterTemplate paths
    , "--output=" ++ postFrontmatterJsonPath paths
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

-- | Rewrite a plain @```lang@ fence opener into the pandoc attribute form
-- that gives Prism's line-numbers/match-braces plugins something to key
-- off of; fences without a language tag are left untouched.
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks =
  unlines . map rewriteLanguageMarkLine . lines
  where
    rewriteLanguageMarkLine :: String -> String
    rewriteLanguageMarkLine line
      | not $ "```" `isPrefixOf` stripped = line
      | mark == "" = line
      | otherwise = indent ++ replaceAll "[mark]" mark "``` {.language-[mark] .line-numbers .match-braces}"
      where
        indent = takeWhile isSpace line
        stripped = dropWhile isSpace line
        mark = trim $ drop 3 stripped
