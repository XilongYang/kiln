module Kiln.Build (kilnBuild) where

import Kiln.Build.Env (BuildEnv (..), logMsg, mkBuildEnv)
import Kiln.Build.FontSubset (subsetFonts)
import Kiln.Build.Page (writePage)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.SearchDb (writeSearchDb)
import Kiln.Config (readConfig)
import Kiln.FileTree (withTempDir)

kilnBuild :: IO ()
kilnBuild = do
  env <- mkBuildEnv <$> readConfig

  withTempDir (envWorkDir env) $ \_ -> do
    logMsg "Rendering posts..."
    results <- renderPosts env
    let entries = map fst results
    logMsg "Writing pages..."
    mapM_ (writePage env entries) (envPages env)
    logMsg "Writing search database..."
    writeSearchDb env results
    logMsg "Subsetting fonts..."
    subsetFonts env entries
    logMsg "Build complete."
