module Kiln.Build (kilnBuild) where

import Kiln.Build.Index (writeIndex)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.Template (loadComponents, substituteComponentsInFile)
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..), readConfig)
import Kiln.FileTree (copyTree, withTempDir)
import System.FilePath ((</>))

tempDirName :: FilePath
tempDirName = ".temp"

templateFiles :: [FilePath]
templateFiles = ["index.html", "post.html"]

kilnBuild :: IO ()
kilnBuild = do
  config <- readConfig
  let templateDir = inTemplate (pathIn (configPath config))
      srcDir = inSrc (pathIn (configPath config))
      postDir = outPost (pathOut (configPath config))
      indexPath = outIndex (pathOut (configPath config))
      webroot = configWebroot config

  withTempDir tempDirName $ \dir -> do
    copyTree templateDir dir
    components <- loadComponents (dir </> "component")
    mapM_ (substituteComponentsInFile dir components) templateFiles
    entries <- renderPosts dir srcDir postDir webroot
    writeIndex dir entries webroot indexPath
