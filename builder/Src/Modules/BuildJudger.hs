module Modules.BuildJudger (shouldBuild) where

import Modules.Config
import Modules.BuildPlan
import Modules.Post
import System.Directory
  ( doesFileExist
  , getModificationTime
  )
import System.FilePath
import Modules.Utils.Files (hashCheck)

-- ---[ Overview ]------------------------------------------------------------
-- | Rebuild decision rules for typed build plans.
--
-- This module centralizes incremental-build policy:
-- - post plans are gated by source hash state and output existence
-- - index plan is gated by metadata-artifact hash state
-- - both plans are additionally gated by builder-source hash state

-- ---[ Public API ]------------------------------------------------------------

-- | Decides whether the given plan should execute.
--
-- Returns 'True' when cache/state checks indicate stale or missing outputs.
shouldBuild :: BuildPlan -> IO Bool
shouldBuild (BuildIndexPlan plan) = indexShouldBuild plan
shouldBuild (BuildPostPlan plan) = postShouldBuild plan

-- ---[ Implementation Details ]-----------------------------------------------
-- | Index rebuild rule.
--
-- Rebuild only when either:
-- - builder sources changed, or
-- - metadata artifact aggregate changed.
indexShouldBuild :: IndexBuildPlan -> IO Bool
indexShouldBuild plan = do
  allCheckPassed <- andM 
    [ builderNotChange
    , targetExists
    , hashCheckPassed
    ]
  return (not allCheckPassed)
  where
    builderNotChange = hashCheck builderPath builderStatePath
    targetExists = doesFileExist $ planIndexHtmlPath plan
    hashCheckPassed = hashCheck metaArtifactsPath metaStatePath
    

-- | Post rebuild rule.
--
-- Rebuild only when any prerequisite is invalid:
-- - builder sources changed
-- - target html missing
-- - source hash differs from stored post state
postShouldBuild :: PostBuildPlan -> IO Bool
postShouldBuild plan = do
  allCheckPassed <- andM 
    [ builderNotChange
    , targetExists
    , srcNotNewerThanTarget
    , hashCheckPassed
    ]
  return (not allCheckPassed)
  where
    srcPath = planPostSourcePath plan
    targetPath = planTargetHtmlPath plan
    statePath = planPostStatePath plan

    builderNotChange = hashCheck builderPath builderStatePath
    targetExists = doesFileExist $ planTargetHtmlPath plan

    srcNotNewerThanTarget = do
      srcTime <- getModificationTime srcPath
      targetTime <- getModificationTime targetPath
      return (srcTime <= targetTime)
    
    hashCheckPassed = hashCheck srcPath statePath
    
andM :: [IO Bool] -> IO Bool
andM [] = return True
andM (x:xs) = do
  cur <- x
  if cur then andM xs else return False
