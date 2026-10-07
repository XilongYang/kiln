module Kiln.Build.Post (renderPosts) where

import Kiln.Build.PostEntry (PostEntry, itemJsonTemplate, loadPostEntry)
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..))
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
  renderPostHtml pageTemplate webroot slug srcPath htmlOutputPath
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
    slug = takeBaseName name
    srcPath = srcDir </> name
    pageTemplate = dir </> "post.html"
    itemTemplatePath = dir </> "item.json.tpl"
    itemJsonPath = dir </> "item.json"
    searchTextPath = dir </> "search-item" </> slug ++ ".txt"
    htmlOutputPath = postDir </> slug ++ ".html"

-- | Run `srcPath` through pandoc using `pageTemplate`, writing the
-- rendered post page to `outputPath`.
renderPostHtml :: FilePath -> String -> String -> FilePath -> FilePath -> IO ()
renderPostHtml pageTemplate webroot slug srcPath outputPath =
  callProcess
    "pandoc"
    [ "--quiet"
    , "--standalone"
    , "--mathjax"
    , "--template=" ++ pageTemplate
    , "--variable=webroot=" ++ webroot
    , "--variable=slug=" ++ slug
    , "--output=" ++ outputPath
    , srcPath
    ]

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
