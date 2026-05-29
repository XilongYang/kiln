module Main where

import Modules.BuildPlan
import Modules.Builder
import Modules.Config
import Modules.FontSubset (genFontSubset)
import Modules.Post
import Modules.SearchDB
import Modules.Template
import Modules.Utils.OrphanCheck (checkOrphans)
import Modules.Utils.TempDir (withTempDir)

import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath

-- | Site build entrypoint.
--
-- Build flow:
-- 1) Prepare isolated temp directory and run orphan checks.
-- 2) Render templates to intermediate files.
-- 3) Ensure post output directory exists.
-- 4) Load and parse all source posts.
-- 5) Build every post page.
-- 6) Build index page from parsed posts.
-- 7) Generate search database.
-- 8) Generate font subset assets.
main :: IO ()
main = withTempDir tempPath $ do
  -- Warning when references point to missing posts/resources.
  checkOrphans

  -- Render template files once so later build plans can consume them.
  templatePost <- genTemplate templatePostPath 
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- genTemplate templateIndexPath 
  writeFile renderedTemplateIndexPath templateIndex

  -- Ensure target directory exists before writing post pages.
  createDirectoryIfMissing True postPath

  -- Parse all source posts into typed post structures.
  postNames <- listDirectory srcPath
  let postPaths = map (\f -> srcPath </> f) postNames 
  posts <- mapM parsePost postPaths

  -- Build each post page.
  let postBuildPlans = map mkBuildPostPlan posts
  mapM_ executeBuildPlan postBuildPlans 

  -- Build index page from the full post list.
  let indexBuildPlan = mkBuildIndexPlan posts
  executeBuildPlan indexBuildPlan 

  -- Generate client-side search index.
  genSearchDB searchDBPath posts 

  -- Subset fonts to reduce shipped asset size.
  genFontSubset 

