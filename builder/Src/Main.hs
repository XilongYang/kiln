module Main where

import Modules.BuildPlan
import Modules.Builder
import Modules.Config
import Modules.FontSubset (genFontSubset)
import Modules.Post
import Modules.Post.Parse
import Modules.SearchDB
import Modules.Template
import Modules.Utils.OrphanCheck (checkOrphans)
import Modules.Utils.TempDir (withTempDir)
import Modules.Utils.Files

import System.Directory (listDirectory, createDirectoryIfMissing, removeFile)
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Site build entrypoint.
--
-- This module wires together the top-level build pipeline:
-- - prepare isolated temp workspace
-- - render templates once per run
-- - execute post and index build plans
-- - aggregate search payload
-- - generate font subset assets
--
-- Directory creation for target and cache outputs is delegated to lower-level
-- writers in 'Modules.Builder' and utility helpers.

-- ---[ Public API ]------------------------------------------------------------

-- | Executes one complete site build in the current workspace.
--
-- Build flow:
-- 1) Run orphan checks.
-- 2) Expand templates into temp files.
-- 3) Build posts and emit per-post artifacts.
-- 4) Build index from metadata artifacts.
-- 5) Concatenate search-item artifacts into @searchdb.json@.
-- 6) Generate subset font from charset artifacts.
-- 7) Update builder source hash state.
main :: IO ()
main = withTempDir tempPath $ do
  -- Warning when references point to missing posts/resources.
  checkOrphans

  -- Render template files once so later build plans can consume them.
  templatePost <- expandTemplate templatePostPath templateComponentPath
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- expandTemplate templateIndexPath templateComponentPath
  writeFile renderedTemplateIndexPath templateIndex

  -- Build each post page.
  postFileNames <- listDirectory srcPath
  let postPaths = map (\f -> srcPath </> f) $ filter (\f -> takeExtension f == ".md") postFileNames 
  let postBuildPlans = map mkBuildPostPlan postPaths
  mapM_ executeBuildPlan postBuildPlans 

  -- Build index page from metadata artifacts emitted by post builds.
  let indexBuildPlan = mkBuildIndexPlan
  executeBuildPlan indexBuildPlan 

  -- Concatenate per-post search items into client-side search index payload.
  writeFile searchDBPath ""
  searchItemFileNames <- listDirectory searchItemArtifactsPath
  let searchItemPaths = map (\f -> searchItemArtifactsPath </> f) $ filter (\f -> takeExtension f == ".klb") searchItemFileNames 
  mapM_ appendSearchItem searchItemPaths

  -- Subset fonts to reduce shipped asset size.
  genFontSubset 

  hashUpdate builderPath builderStatePath

-- ---[ Implementation Details ]-----------------------------------------------

-- | Appends one serialized search-item KLB block into @searchdb.json@.
--
-- The current search payload is a plain concatenation of per-post KLB files.
appendSearchItem :: FilePath -> IO ()
appendSearchItem path = do
  item <- readFile path
  appendFile searchDBPath item
