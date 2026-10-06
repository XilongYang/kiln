module Kiln.Build (kilnBuild) where

import Kiln.Build.FontSubset (subsetFonts)
import Kiln.Build.Index (writeIndex)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.SearchDb (writeSearchDb)
import Kiln.Build.Template (loadComponents, substituteComponentsInFile)
import Kiln.Config (InPaths (..), KilnConfig (..), PathConfig (..), readConfig)
import Kiln.FileTree (copyTree, withTempDir)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stdout)

tempDirName :: FilePath
tempDirName = ".temp"

templateFiles :: [FilePath]
templateFiles = ["index.html", "post.html"]

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
    components <- loadComponents (dir </> "component")
    logMsg "Substituting components..."
    mapM_ (substituteComponentsInFile dir components) templateFiles
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
