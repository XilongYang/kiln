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
-- - post plans are gated by source hash state, mtime ordering, and output existence
-- - index plan is gated by metadata-artifact hash state and output existence

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
-- - index target missing, or
-- - metadata artifact aggregate changed.
indexShouldBuild :: IndexBuildPlan -> IO Bool
indexShouldBuild plan = do
  allCheckPassed <- andM 
    [ targetExists
    , hashCheckPassed
    ]
  return (not allCheckPassed)
  where
    targetExists = doesFileExist $ planIndexHtmlPath plan
    hashCheckPassed = hashCheck metaArtifactsPath metaStatePath
    

-- | Post rebuild rule.
--
-- Rebuild only when any prerequisite is invalid:
-- - target html missing
-- - source file mtime is newer than target html
-- - source hash differs from stored post state
postShouldBuild :: PostBuildPlan -> IO Bool
postShouldBuild plan = do
  allCheckPassed <- andM 
    [ targetExists
    , srcNotNewerThanTarget
    , hashCheckPassed
    ]
  return (not allCheckPassed)
  where
    srcPath = planPostSourcePath plan
    targetPath = planTargetHtmlPath plan
    statePath = planPostStatePath plan

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
