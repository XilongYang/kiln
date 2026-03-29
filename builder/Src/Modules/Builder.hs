module Modules.Builder (executeBuildPlan) where

import Modules.BuildPlan
import Modules.BuildJudger
import Modules.Index.Render
import Modules.Post.Preprocess
import Modules.Post.Parse
import Modules.Pandoc
import Modules.Toc

-- ---[ Overview ]------------------------------------------------------------
-- | Build-plan executor for post and index generation.
--
-- This module dispatches typed build plans and runs the corresponding
-- filesystem/render pipeline.

-- ---[ Public API ]------------------------------------------------------------

-- | Executes a build plan when it is marked as needing rebuild.
--
-- Delegates to plan-specific executors for post and index targets.
executeBuildPlan :: BuildPlan -> IO ()
executeBuildPlan plan = do
  isShouldBuild <- shouldBuild plan
  if not isShouldBuild then return () else do
    realExecuteBuildPlan plan    

-- ---[ Implementation Details ]-----------------------------------------------

-- | Dispatches the concrete build action by plan constructor.
realExecuteBuildPlan :: BuildPlan -> IO ()
realExecuteBuildPlan (BuildPostPlan plan) = do
  buildPostWithPlan plan
realExecuteBuildPlan (BuildIndexPlan plan) = do
  buildIndexWithPlan plan

-- | Builds @index.html@ from prepared index items and template HTML.
buildIndexWithPlan :: IndexBuildPlan -> IO ()
buildIndexWithPlan plan = do
  let indexItems = planIndexItems plan
  let indexTemplatePath = planIndexTemplatePath plan
  indexTemplateHtml <- readFile indexTemplatePath
  let indexHtmlPath = planIndexHtmlPath plan
  let indexHtml = renderIndex indexItems indexTemplateHtml 
  writeFile indexHtmlPath indexHtml
  
-- | Builds one post HTML page from a post build plan.
--
-- Pipeline:
-- - preprocess markdown content
-- - render via pandoc and post template
-- - inject TOC into generated HTML
-- - write final target page
buildPostWithPlan :: PostBuildPlan -> IO()
buildPostWithPlan plan = do
  let preprocessedPath = planPreprocessedPath plan
  let builtHtmlPath = planBuiltHtmlPath plan
  let postTemplatePath = planPostTemplatePath plan
  let targetHtmlPath = planTargetHtmlPath plan
  post <- parsePost $ planPostSourcePath plan
  writeFile preprocessedPath $ preprocessPost post
  runPandoc preprocessedPath postTemplatePath builtHtmlPath
  builtHtml <- readFile builtHtmlPath
  writeFile targetHtmlPath $ injectToc builtHtml
