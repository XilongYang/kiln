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

import System.Directory (listDirectory)
import System.FilePath

-- | Site build entrypoint.
--
-- Build flow:
-- 1) Prepare isolated temp directory and run orphan checks.
-- 2) Render templates to intermediate files.
-- 3) Build every post from source files and emit per-post artifacts.
-- 4) Build index page from metadata artifacts.
-- 5) Concatenate search-item artifacts into search database payload.
-- 6) Build font subset from charset artifacts.
--
-- Directory creation for post/html/cache artifacts is handled in
-- 'Modules.Builder' when files are materialized.
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
  searchItemFileNames <- listDirectory searchItemArtifactsPath
  let searchItemPaths = map (\f -> searchItemArtifactsPath </> f) $ filter (\f -> takeExtension f == ".klb") searchItemFileNames 
  mapM_ appendSearchItem searchItemPaths

  -- Subset fonts to reduce shipped asset size.
  genFontSubset 

-- | Appends one serialized search-item KLB block into @searchdb.json@.
--
-- The current search payload is a plain concatenation of per-post KLB files.
appendSearchItem :: FilePath -> IO ()
appendSearchItem path = do
  item <- readFile path
  appendFile searchDBPath item
