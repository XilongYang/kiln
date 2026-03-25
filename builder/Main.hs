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

import System.Directory (listDirectory)
import System.FilePath

-- Build entrypoint:
-- 1) recreate temp workspace;
-- 2) report generated post pages that no longer have source markdown;
-- 3) render templates into temp files for downstream steps.
main :: IO ()
main = withTempDir tempPath $ do
  checkOrphans

  templatePost <- genTemplate templatePostPath 
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- genTemplate templateIndexPath 
  writeFile renderedTemplateIndexPath templateIndex

  postNames <- listDirectory srcPath
  let postPaths = map (\f -> srcPath </> f) postNames 
  posts <- mapM parsePost postPaths

  let postBuildPlans = map mkBuildPostPlan posts
  mapM_ executeBuildPlan postBuildPlans 

  let indexBuildPlan = mkBuildIndexPlan posts
  executeBuildPlan indexBuildPlan 

  genSearchDB searchDBPath posts 

  genFontSubset 

