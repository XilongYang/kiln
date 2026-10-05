module Kiln.Clean (kilnClean) where

import Control.Monad (when)
import Kiln.Config (KilnConfig (..), OutPaths (..), PathConfig (..), readConfig)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )

kilnClean :: IO ()
kilnClean = do
  config <- readConfig
  mapM_ removePath (outPaths (pathOut (configPath config)))

outPaths :: OutPaths -> [FilePath]
outPaths out =
  [ outPost out
  , outFontsSubset out
  , outSearchDb out
  , outIndex out
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
