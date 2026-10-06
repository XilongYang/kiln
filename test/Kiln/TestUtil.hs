module Kiln.TestUtil (withTempDir) where

import qualified Kiln.FileTree as FileTree
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

-- | Create a fresh empty directory under the system temp root for the
-- duration of the action, then remove it afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tmpRoot <- getTemporaryDirectory
  (path, h) <- openTempFile tmpRoot "kiln-test-"
  hClose h
  removeFile path
  FileTree.withTempDir path action
