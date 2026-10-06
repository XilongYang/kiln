{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build (kilnBuild) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..), readConfig)
import Kiln.FileTree (copyTree, withTempDir)
import Kiln.Index (PostEntry (..), renderPostsList)
import Kiln.Template (renderComponents, replaceAll)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (readFile')
import System.Process (callProcess)

tempDirName :: FilePath
tempDirName = ".temp"

indexItemsDirName :: FilePath
indexItemsDirName = "index-items"

templateFiles :: [FilePath]
templateFiles = ["index.html", "post.html"]

itemJsonTemplate :: String
itemJsonTemplate = "{\"title\": \"$title$\", \"date\": \"$date$\"}"

kilnBuild :: IO ()
kilnBuild = do
  config <- readConfig
  withTempDir tempDirName $ \dir -> do
    copyTree (inTemplate (pathIn (configPath config))) dir
    components <- loadComponents (dir </> "component")
    mapM_ (substituteComponentsInFile dir components) templateFiles
    renderPosts dir (inSrc (pathIn (configPath config))) (outPost (pathOut (configPath config))) (configWebroot config)
    renderIndex dir (outIndex (pathOut (configPath config))) (configWebroot config)

loadComponents :: FilePath -> IO [(String, String)]
loadComponents dir = do
  names <- listDirectory dir
  mapM (\name -> (,) (takeBaseName name) <$> readFile' (dir </> name)) names

substituteComponentsInFile :: FilePath -> [(String, String)] -> FilePath -> IO ()
substituteComponentsInFile dir components name = do
  let path = dir </> name
  content <- readFile' path
  writeFile path (renderComponents components content)

-- | Render every markdown post under `srcDir` through the (already
-- component-substituted) post.html template into `postDir`, and drop a
-- `<slug>-item.json` summary of each post's title/date into
-- `dir/index-items` for `renderIndex` to pick up afterwards.
renderPosts :: FilePath -> FilePath -> FilePath -> String -> IO ()
renderPosts dir srcDir postDir webroot = do
  names <- listDirectory srcDir
  let mdNames = filter ((== ".md") . takeExtension) names
      itemsDir = dir </> indexItemsDirName
      itemTemplatePath = dir </> "item.json.tpl"
  createDirectoryIfMissing True postDir
  createDirectoryIfMissing True itemsDir
  writeFile itemTemplatePath itemJsonTemplate
  mapM_ (renderPost srcDir postDir (dir </> "post.html") itemsDir itemTemplatePath webroot) mdNames

renderPost :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> String -> FilePath -> IO ()
renderPost srcDir postDir pageTemplate itemsDir itemTemplate webroot name = do
  callProcess
    "pandoc"
    [ "--standalone"
    , "--template=" ++ pageTemplate
    , "--variable=webroot=" ++ webroot
    , "--output=" ++ (postDir </> slug ++ ".html")
    , srcPath
    ]
  callProcess
    "pandoc"
    [ "--standalone"
    , "--to=plain"
    , "--template=" ++ itemTemplate
    , "--output=" ++ (itemsDir </> slug ++ "-item.json")
    , srcPath
    ]
  where
    slug = takeBaseName name
    srcPath = srcDir </> name

-- | Fill in the `$posts$` placeholder of the (already
-- component-substituted) index.html template using the per-post summaries
-- `renderPosts` collected, and write the result to the configured index
-- output path.
renderIndex :: FilePath -> FilePath -> String -> IO ()
renderIndex dir outIndexPath webroot = do
  let itemsDir = dir </> indexItemsDirName
  itemNames <- listDirectory itemsDir
  entries <- mapM (loadPostEntry itemsDir) itemNames
  content <- readFile' (dir </> "index.html")
  let withPosts = replaceAll "$posts$" (renderPostsList webroot entries) content
  writeFile outIndexPath (replaceAll "$webroot$" webroot withPosts)

data PostItem = PostItem { itemTitle :: String, itemDate :: String }

instance FromJSON PostItem where
  parseJSON = withObject "post-item" $ \o ->
    PostItem <$> o .: "title" <*> o .: "date"

loadPostEntry :: FilePath -> FilePath -> IO PostEntry
loadPostEntry itemsDir name = do
  result <- eitherDecodeFileStrict (itemsDir </> name)
  item <- either fail pure result
  pure PostEntry { postTitle = itemTitle item, postDate = itemDate item, postSlug = slug }
  where
    slug = take (length name - length ("-item.json" :: String)) name
