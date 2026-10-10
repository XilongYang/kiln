module Kiln.Clean (kilnClean) where

import Control.Monad (when)
import Kiln.Config (FontsConfig (..), KilnConfig (..), PageConfig (..), PostPageConfig (..), TargetConfig (..), readConfig)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )

kilnClean :: IO ()
kilnClean = do
  config <- readConfig
  let target = configTarget config
  mapM_
    removePath
    ( fixedOutPaths target
        ++ maybe [] ((: []) . fontsSubsetDir) (targetFonts target)
        ++ maybe [] ((: []) . postPageOutputDir) (targetPost target)
        ++ map pageOutput (targetPages target)
    )

fixedOutPaths :: TargetConfig -> [FilePath]
fixedOutPaths target =
  [ targetSearchDb target
  , targetCacheDir target
  ]

removePath :: FilePath -> IO ()
removePath path = do
  isDir <- doesDirectoryExist path
  if isDir
    then removeDirectoryRecursive path
    else do
      isFile <- doesFileExist path
      when isFile $ removeFile path
