module Kiln.TestUtil (withTempDir) where

import Control.Exception (bracket)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.IO (hClose, openTempFile)

-- | Create a fresh empty directory for the duration of the action, then
-- remove it (and everything written into it) afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      tmpRoot <- getTemporaryDirectory
      (path, h) <- openTempFile tmpRoot "kiln-test-"
      hClose h
      removeFile path
      createDirectory path
      pure path
