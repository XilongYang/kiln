module Kiln.Clean (kilnClean) where

import Control.Monad (when)
import Kiln.Config (FontsConfig (..), KilnConfig (..), OutPaths (..), PageConfig (..), PathConfig (..), PostPageConfig (..), readConfig)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )

kilnClean :: IO ()
kilnClean = do
  config <- readConfig
  mapM_
    removePath
    ( fixedOutPaths (pathOut (configPath config))
        ++ maybe [] ((: []) . fontsSubsetPath) (configFonts config)
        ++ maybe [] ((: []) . postPageOutput) (configPost config)
        ++ map pageOutput (configPages config)
    )

fixedOutPaths :: OutPaths -> [FilePath]
fixedOutPaths out =
  [ outSearchDb out
  , outCache out
  ]

removePath :: FilePath -> IO ()
removePath path = do
  isDir <- doesDirectoryExist path
  if isDir
    then removeDirectoryRecursive path
    else do
      isFile <- doesFileExist path
      when isFile $ removeFile path
