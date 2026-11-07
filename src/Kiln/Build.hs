module Kiln.Build (kilnBuild) where

import Kiln.Build.Env (BuildEnv (..), logMsg, mkBuildEnv)
import Kiln.Build.FontSubset (subsetFonts)
import Kiln.Build.Page (writePage)
import Kiln.Build.Post (renderPosts)
import Kiln.Build.SearchDb (writeSearchDb)
import Kiln.Config (readConfig)
import Kiln.FileTree (withTempDir)
import System.IO (BufferMode (..), hSetBuffering, stderr)

kilnBuild :: IO ()
kilnBuild = do
  -- Lines, not characters: stderr is unbuffered by default, so the
  -- concurrent stages' logMsg would interleave mid-word.
  hSetBuffering stderr LineBuffering
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
