module Kiln.FileTree (copyTree, withTempDir) where

import Control.Exception (bracket)
import Control.Monad (when)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getPermissions
  , listDirectory
  , removeDirectoryRecursive
  , setOwnerWritable
  , setPermissions
  )
import System.FilePath ((</>))

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
    else do
      -- copyFile also copies the source's permissions; if the source
      -- (e.g. a Nix store path) is read-only, make sure the copy isn't.
      copyFile srcPath dstPath
      permissions <- getPermissions dstPath
      setPermissions dstPath (setOwnerWritable True permissions)
  where
    srcPath = src </> name
    dstPath = dst </> name

-- | Make `path` a fresh empty directory for the duration of the action,
-- removing any stale contents first and cleaning up afterwards.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir path = bracket acquire (const (removeDirectoryRecursive path))
  where
    acquire = do
      exists <- doesDirectoryExist path
      when exists $ removeDirectoryRecursive path
      createDirectoryIfMissing True path
      pure path
