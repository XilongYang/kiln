module Kiln.Build (kilnBuild) where

import Kiln.Build.Index (writeIndex)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.SearchDb (writeSearchDb)
import Kiln.Build.Template (loadComponents, substituteComponentsInFile)
import Kiln.Config (InPaths (..), KilnConfig (..), PathConfig (..), readConfig)
import Kiln.FileTree (copyTree, withTempDir)
import System.FilePath ((</>))

tempDirName :: FilePath
tempDirName = ".temp"

templateFiles :: [FilePath]
templateFiles = ["index.html", "post.html"]

kilnBuild :: IO ()
kilnBuild = do
  config <- readConfig
  let inPaths = pathIn (configPath config)
      templateDir = inTemplate inPaths

  withTempDir tempDirName $ \dir -> do
    copyTree templateDir dir
    components <- loadComponents (dir </> "component")
    mapM_ (substituteComponentsInFile dir components) templateFiles
    results <- renderPosts config dir
    writeIndex config dir (map fst results)
    writeSearchDb config results
