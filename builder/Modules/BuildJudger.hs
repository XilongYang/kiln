module Modules.BuildJudger (shouldBuild) where

import Modules.Config
import Modules.BuildPlan
import Modules.Post
import System.Directory
  ( doesFileExist
  , getModificationTime
  )
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Build-decision rules for typed build plans.
--
-- This module evaluates whether a build plan should run, based on plan kind
-- and source/target file timestamps.

-- ---[ Public API ]------------------------------------------------------------

-- | Decides whether the given plan should execute.
--
-- Index plan currently always rebuilds; post plan is timestamp-based.
shouldBuild :: BuildPlan -> IO Bool
shouldBuild (BuildIndexPlan _) = return True
shouldBuild (BuildPostPlan plan) = postShouldBuild plan

-- ---[ Implementation Details ]-----------------------------------------------

-- | Rebuild check for post plan:
-- - rebuild if target html is missing
-- - rebuild if source markdown is newer than target html
postShouldBuild :: PostBuildPlan -> IO Bool
postShouldBuild plan = do
  let srcPath = postSourcePath $ planPost plan
  let targetPath = planTargetHtmlPath plan

  targetExists <- doesFileExist $ planTargetHtmlPath plan
  if not targetExists 
    then return True
    else do
      srcTime <- getModificationTime srcPath
      targetTime <- getModificationTime targetPath
      return (srcTime > targetTime)
