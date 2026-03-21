module Modules.Utils.TempDir where

import Modules.Config

import Control.Exception (bracket)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Directory (doesDirectoryExist)

-- Runs an action inside a freshly recreated temporary directory.
-- Any existing directory at `tempPath` is removed first, and the
-- directory is always cleaned up after the action finishes.
withTempDir :: IO a -> IO a
withTempDir action = bracket
  (do
      -- Keep the temp directory deterministic by removing leftovers
      -- from a previous run before creating it again.
      exists <- doesDirectoryExist tempPath
      if exists then removeDirectoryRecursive tempPath else return ()
      createDirectory tempPath
      return tempPath
  )
  -- `bracket` guarantees cleanup even if the action throws.
  (\d -> removeDirectoryRecursive d)
  (\_ -> action)
