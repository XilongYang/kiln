module Modules.Utils.TempDir where

import Control.Exception (bracket)
import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive)

-- ---[ Overview ]------------------------------------------------------------
-- | Temporary directory lifecycle helper for build tasks.
--
-- This module provides a safe wrapper that recreates a working directory
-- before running an action and always removes it afterwards.

-- ---[ Public API ]------------------------------------------------------------

-- | Runs an action inside a freshly recreated temporary directory.
--
-- Behavior:
-- - removes existing directory at target path (if present)
-- - creates a new empty directory
-- - guarantees cleanup after action finishes (success or exception)
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

