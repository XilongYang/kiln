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

-- | Render every markdown post in `config`'s source directory through the
-- (already component-substituted) `dir </> "post.html"` template into
-- `config`'s post output directory. Returns each post's `PostEntry`
-- summary (for the index) paired with its plain-text content (for the
-- search database).
renderPosts :: KilnConfig -> FilePath -> IO [(PostEntry, String)]
renderPosts config dir = do
  names <- listDirectory srcDir
  let mdNames = filter ((== ".md") . takeExtension) names
  createDirectoryIfMissing True postDir
  createDirectoryIfMissing True searchItemDir
  writeFile itemTemplatePath itemJsonTemplate
  mapM (renderPost config dir) mdNames
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    postDir = outPost outPaths
    searchItemDir = dir </> "search-item"
    itemTemplatePath = dir </> "item.json.tpl"

renderPost :: KilnConfig -> FilePath -> FilePath -> IO (PostEntry, String)
renderPost config dir name = do
  src <- readFile' srcPath
  writeFile rewrittenPath (rewriteLanguageMarks src)
  renderPostHtml (tocEnable toc) (tocDepth toc) (tocNumberSections toc) pageTemplate webroot slug rewrittenPath htmlOutputPath
  renderPostItemJson itemTemplatePath slug srcPath itemJsonPath
  renderPostSearchText srcPath searchTextPath
  entry <- loadPostEntry itemJsonPath
  content <- readFile' searchTextPath
  pure (entry, content)
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    srcDir = inSrc inPaths
    postDir = outPost outPaths
    webroot = configWebroot config
    toc = configToc config
    slug = takeBaseName name
    srcPath = srcDir </> name
    rewrittenPath = dir </> name
    pageTemplate = dir </> "post.html"
    itemTemplatePath = dir </> "item.json.tpl"
    itemJsonPath = dir </> "item.json"
    searchTextPath = dir </> "search-item" </> slug ++ ".txt"
    htmlOutputPath = postDir </> slug ++ ".html"

-- | Run `srcPath` through pandoc using `pageTemplate`, writing the
-- rendered post page to `outputPath`.
renderPostHtml :: Bool -> Int -> Bool -> FilePath -> String -> String -> FilePath -> FilePath -> IO ()
renderPostHtml enableToc tocDepth numberSections pageTemplate webroot slug srcPath outputPath =
  callProcess
    "pandoc"
    ( [ "--quiet"
      , "--standalone"
      , "--mathjax"
      , "--template=" ++ pageTemplate
      , "--variable=webroot=" ++ webroot
      , "--variable=slug=" ++ slug
      , "--output=" ++ outputPath
      , srcPath
      ]
      ++ tocFlags
    )
  where
    tocFlags
      | enableToc = ["--toc", "--toc-depth=" ++ show tocDepth] ++ ["--number-sections" | numberSections]
      | otherwise = []

-- | Run `srcPath` through pandoc using `itemTemplate` (see
-- `itemJsonTemplate`) to extract its title/date/slug as JSON, written to
-- `outputPath`.
renderPostItemJson :: FilePath -> String -> FilePath -> FilePath -> IO ()
renderPostItemJson itemTemplate slug srcPath outputPath =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--standalone"
    , "--to=plain"
    , "--wrap=none"
    , "--template=" ++ itemTemplate
    , "--variable=slug=" ++ slug
    , "--output=" ++ outputPath
    , srcPath
    ]

-- | Run `srcPath` through pandoc's plain writer (no template, just the
-- body) for the search database to index, written to `outputPath`.
renderPostSearchText :: FilePath -> FilePath -> IO ()
renderPostSearchText srcPath outputPath =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--to=plain"
    , "--wrap=none"
    , "--output=" ++ outputPath
    , srcPath
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
