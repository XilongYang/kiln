module Modules.Builder where

import Modules.BuildPlan
import Modules.Builder.IndexBuilder
import Modules.Builder.PostBuilder

-- Executes one build plan with incremental guard.
--
-- Plans that do not need rebuild are skipped.
executeBuildPlan :: BuildPlan -> IO ()
executeBuildPlan plan = do
  isShouldBuild <- shouldBuild plan
  if not isShouldBuild then return () else do
    realExecuteBuildPlan plan    

-- Dispatches to concrete build actions for each plan type.
realExecuteBuildPlan :: BuildPlan -> IO ()
realExecuteBuildPlan (BuildPostPlan plan) = do
  buildPostWithPlan plan
realExecuteBuildPlan (BuildIndexPlan plan) = do
  buildIndexWithPlan plan

-- Writes preprocessed markdown for one post build plan.
--
-- The generated markdown is consumed by the downstream renderer.
buildPostWithPlan :: PostBuildPlan -> IO()
buildPostWithPlan plan = do
  let post = planPost plan
  let preprocessedPath = planPreprocessedPath plan
  let builtHtmlPath = planBuiltHtmlPath plan
  let postTemplatePath = planPostTemplatePath plan
  let targetHtmlPath = planTargetHtmlPath plan
  writeFile preprocessedPath $ genPreprocessedPostText post
  runPandoc preprocessedPath postTemplatePath builtHtmlPath
  builtHtml <- readFile builtHtmlPath
  writeFile targetHtmlPath $ genToc builtHtml

-- Renders the index page from prepared index items and a rendered template.
--
-- The template must contain the `$posts$` placeholder, which will be replaced
-- by grouped/sorted post links markup.
buildIndexWithPlan :: IndexBuildPlan -> IO ()
buildIndexWithPlan plan = do
  let indexItems = planIndexItems plan
  let indexTemplatePath = planIndexTemplatePath plan
  indexTemplateHtml <- readFile indexTemplatePath
  let indexHtmlPath = planIndexHtmlPath plan
  let indexHtml = genIndexHtml indexItems indexTemplateHtml 
  writeFile indexHtmlPath indexHtml
  
