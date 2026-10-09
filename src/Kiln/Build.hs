module Kiln.Build (kilnBuild) where

import Kiln.Build.FontSubset (subsetFonts)
import Kiln.Build.Index (writeIndex)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.SearchDb (writeSearchDb)
import Kiln.Config (InPaths (..), KilnConfig (..), PathConfig (..), readConfig)
import Kiln.FileTree (copyTree, withTempDir)
import System.IO (hPutStrLn, stdout)

tempDirName :: FilePath
tempDirName = ".temp"

logMsg :: String -> IO ()
logMsg = hPutStrLn stdout

kilnBuild :: IO ()
kilnBuild = do
  config <- readConfig
  let inPaths = pathIn (configPath config)
      templateDir = inTemplate inPaths

  withTempDir tempDirName $ \dir -> do
    logMsg "Copying template..."
    copyTree templateDir dir
    logMsg "Rendering posts..."
    results <- renderPosts config dir
    let entries = map fst results
    logMsg "Writing index..."
    writeIndex config dir entries
    logMsg "Writing search database..."
    writeSearchDb config results
    logMsg "Subsetting fonts..."
    subsetFonts config entries
    logMsg "Build complete."
