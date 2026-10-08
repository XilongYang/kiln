module Kiln.Build.Post (renderPosts) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Kiln.Build.PostEntry (PostEntry, itemJsonTemplate, loadPostEntry)
import Kiln.Build.Template (replaceAll)
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..), TocConfig (..))
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | Metadata shared across a post's render steps that isn't itself a
-- filesystem path: its slug, the site's webroot, and the table-of-contents
-- settings.
data PostMeta = PostMeta
  { postSlug    :: String
  , postWebroot :: String
  , postToc     :: TocConfig
  }

-- | Every filesystem path touched while rendering one post. Grouping these
-- avoids passing a run of same-typed `FilePath`/`String` arguments
-- positionally, where it's easy to pass them in the wrong order.
data PostPaths = PostPaths
  { postSrcPath        :: FilePath -- ^ the post's original markdown
  , postRewrittenPath  :: FilePath -- ^ language-mark-rewritten body fed to pandoc
  , postPageTemplate   :: FilePath -- ^ the post page template, @post.html@
  , postItemTemplate   :: FilePath -- ^ the item JSON template, @item.json.tpl@
  , postItemJsonPath   :: FilePath -- ^ the rendered item JSON, for the index
  , postSearchTextPath :: FilePath -- ^ the rendered plain text, for the search database
  , postHtmlOutputPath :: FilePath -- ^ the rendered post page
  , postTocTemplate    :: FilePath -- ^ the bare @$toc$@ template, @toc.tpl@
  , postTocHtmlPath    :: FilePath -- ^ the rendered standalone toc fragment
  }

-- | Render every markdown post in `config`'s source directory through the
-- (already component-substituted) `tempDir </> "post.html"` template into
-- `config`'s post output directory. Returns each post's `PostEntry`
-- summary (for the index) paired with its plain-text content (for the
-- search database).
renderPosts :: KilnConfig -> FilePath -> IO [(PostEntry, String)]
renderPosts config tempDir = do
  names <- listDirectory srcDir
  let mdNames = filter ((== ".md") . takeExtension) names
  createDirectoryIfMissing True postDir
  createDirectoryIfMissing True searchItemDir
  writeFile itemTemplatePath itemJsonTemplate
  writeFile tocTemplatePath "$toc$"
  mapM (renderPost config tempDir) mdNames
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    postDir = outPost outPaths
    searchItemDir = tempDir </> "search-item"
    itemTemplatePath = tempDir </> "item.json.tpl"
    tocTemplatePath = tempDir </> "toc.tpl"

renderPost :: KilnConfig -> FilePath -> FilePath -> IO (PostEntry, String)
renderPost config tempDir name = do
  src <- readFile' (postSrcPath paths)
  let (abstractSrc, bodySrc) = splitAbstract (rewriteLanguageMarks src)
  writeFile (postRewrittenPath paths) bodySrc
  abstractHtml <- traverse (renderAbstractHtml tempDir) abstractSrc
  renderPostHtml meta paths abstractHtml
  renderPostItemJson meta paths
  renderPostSearchText paths
  entry <- loadPostEntry (postItemJsonPath paths)
  content <- readFile' (postSearchTextPath paths)
  pure (entry, content)
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    postDir = outPost outPaths
    slug = takeBaseName name
    meta =
      PostMeta
        { postSlug = slug
        , postWebroot = configWebroot config
        , postToc = configToc config
        }
    paths =
      PostPaths
        { postSrcPath = srcDir </> name
        , postRewrittenPath = tempDir </> name
        , postPageTemplate = tempDir </> "post.html"
        , postItemTemplate = tempDir </> "item.json.tpl"
        , postItemJsonPath = tempDir </> "item.json"
        , postSearchTextPath = tempDir </> "search-item" </> slug ++ ".txt"
        , postHtmlOutputPath = postDir </> slug ++ ".html"
        , postTocTemplate = tempDir </> "toc.tpl"
        , postTocHtmlPath = tempDir </> slug ++ "-toc.html"
        }

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

-- | Run the rewritten post body through pandoc using the page template,
-- writing the rendered post page. The table of contents, if enabled, is
-- rendered separately (see `renderTocHtml`) so its @<ul>@s can be
-- rewritten into @<ol>@s before being spliced in as the @toc@ variable;
-- pandoc's own `--toc` flag only ever emits @<ul>@s.
renderPostHtml :: PostMeta -> PostPaths -> Maybe String -> IO ()
renderPostHtml meta paths abstractHtml = do
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
      , "--template=" ++ postPageTemplate paths
      , "--variable=webroot=" ++ postWebroot meta
      , "--variable=slug=" ++ postSlug meta
      , "--output=" ++ postHtmlOutputPath paths
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

-- | Run the post's source through pandoc using the item template (see
-- `itemJsonTemplate`) to extract its title/date/slug as JSON.
renderPostItemJson :: PostMeta -> PostPaths -> IO ()
renderPostItemJson meta paths =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--standalone"
    , "--to=plain"
    , "--wrap=none"
    , "--template=" ++ postItemTemplate paths
    , "--variable=slug=" ++ postSlug meta
    , "--output=" ++ postItemJsonPath paths
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

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
