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
      -- copyFile carries the source's permissions over, and `Kiln.Init`
      -- reads from the read-only Nix store. Keep the copy writable.
      copyFile srcPath dstPath
      permissions <- getPermissions dstPath
      setPermissions dstPath (setOwnerWritable True permissions)
  where
    srcPath = src </> name
    dstPath = dst </> name

-- | Make `path` a fresh empty directory for the action, cleaning up
-- afterwards. Unconditionally destructive at both ends, so point it only
-- at a directory kiln owns -- the caller passes
-- `Kiln.Build.Env.envWorkDir`, under the cache directory for that
-- reason.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir path = bracket acquire (const (removeDirectoryRecursive path))
  where
    acquire = do
      exists <- doesDirectoryExist path
      when exists $ removeDirectoryRecursive path
      createDirectoryIfMissing True path
      pure path
