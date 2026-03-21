module Modules.Utils.TempDir where

import Control.Exception (bracket)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Directory (doesDirectoryExist)

-- Runs an action inside a freshly recreated temporary directory.
-- Any existing directory at the target path is removed first, and the
-- directory is always cleaned up after the action finishes.
withTempDir :: FilePath -> IO a -> IO a
withTempDir dirPath action = bracket
  (do
      exists <- doesDirectoryExist dirPath
      if exists then removeDirectoryRecursive dirPath else return ()
      createDirectory dirPath
      return dirPath
  )
  -- `bracket` guarantees cleanup even if the action throws.
  (\d -> removeDirectoryRecursive d)
  (\_ -> action)
