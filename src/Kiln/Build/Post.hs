module Kiln.Build.Post (renderPosts) where

import Kiln.Build.PostEntry (PostEntry, itemJsonTemplate, loadPostEntry)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Process (callProcess)

-- | Render every markdown post under `srcDir` through the (already
-- component-substituted) `post.html` template at `dir </> "post.html"`
-- into `postDir`, prefixing generated links with `webroot`. Returns a
-- `PostEntry` summary of each post for the index to list.
renderPosts :: FilePath -> FilePath -> FilePath -> String -> IO [PostEntry]
renderPosts dir srcDir postDir webroot = do
  names <- listDirectory srcDir
  let mdNames = filter ((== ".md") . takeExtension) names
      itemTemplatePath = dir </> "item.json.tpl"
      itemJsonPath = dir </> "item.json"
  createDirectoryIfMissing True postDir
  writeFile itemTemplatePath itemJsonTemplate
  mapM (renderPost srcDir postDir (dir </> "post.html") itemTemplatePath itemJsonPath webroot) mdNames

renderPost :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> String -> FilePath -> IO PostEntry
renderPost srcDir postDir pageTemplate itemTemplate itemJsonPath webroot name = do
  renderPostHtml pageTemplate webroot srcPath (postDir </> slug ++ ".html")
  renderPostItemJson itemTemplate slug srcPath itemJsonPath
  loadPostEntry itemJsonPath
  where
    slug = takeBaseName name
    srcPath = srcDir </> name

-- | Run `srcPath` through pandoc using `pageTemplate`, writing the
-- rendered post page to `outputPath`.
renderPostHtml :: FilePath -> String -> FilePath -> FilePath -> IO ()
renderPostHtml pageTemplate webroot srcPath outputPath =
  callProcess
    "pandoc"
    [ "--standalone"
    , "--template=" ++ pageTemplate
    , "--variable=webroot=" ++ webroot
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
    [ "--standalone"
    , "--to=plain"
    , "--wrap=none"
    , "--template=" ++ itemTemplate
    , "--variable=slug=" ++ slug
    , "--output=" ++ outputPath
    , srcPath
    ]
