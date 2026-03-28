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
  templatePost <- expandTemplate templatePostPath templateComponentPath
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- expandTemplate templateIndexPath templateComponentPath
  writeFile renderedTemplateIndexPath templateIndex

  -- Ensure target directory exists before writing post pages.
  createDirectoryIfMissing True postPath

  -- Build each post page.
  postFileNames <- listDirectory srcPath
  let postPaths = map (\f -> srcPath </> f) $ filter (\f -> takeExtension f == ".md") postFileNames 
  let postBuildPlans = map mkBuildPostPlan postPaths
  mapM_ executeBuildPlan postBuildPlans 

  -- Build index page from the full post list.
  -- let indexBuildPlan = mkBuildIndexPlan posts
  -- executeBuildPlan indexBuildPlan 

  -- Generate client-side search index.
  -- genSearchDB searchDBPath posts 

  -- Subset fonts to reduce shipped asset size.
  -- genFontSubset 
