module Kiln.Init (kilnInit) where

import Control.Monad (unless)
import Paths_kiln (getDataDir)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , listDirectory
  )
import System.Exit (die)
import System.FilePath ((</>))

kilnInit :: IO ()
kilnInit = do
  empty <- isEmptyDir "."
  unless empty $ die "Directory is not empty"
  dataDir <- getDataDir
  copyTree (dataDir </> "template" </> "hello-kiln") "."

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  mapM_ (copyEntry src dst) entries

copyEntry :: FilePath -> FilePath -> FilePath -> IO ()
copyEntry src dst name = do
  isDir <- doesDirectoryExist srcPath
  if isDir
    then copyTree srcPath dstPath
    else copyFile srcPath dstPath
  where
    srcPath = src </> name
    dstPath = dst </> name

isEmptyDir :: FilePath -> IO Bool
isEmptyDir p = null <$> listDirectory p
