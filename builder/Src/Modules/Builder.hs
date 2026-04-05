module Modules.Builder (executeBuildPlan) where

import Modules.BuildPlan
import Modules.BuildJudger
import Modules.Index.Render
import Modules.Post.Preprocess
import Modules.Post.Parse
import Modules.Pandoc
import Modules.Toc
import Modules.Index.Item (mkIndexItem)
import Modules.Post (Post(postMeta))
import Modules.Utils.Klb
import Modules.Config (tempIndexItemsKlbPath)

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
  indexItemsKlbStr <- readFile tempIndexItemsKlbPath
  let eitherIndexItems = parseKlb indexItemsKlbStr
  case eitherIndexItems of
    Left e -> do
      putStrLn $ "[Error] parse KLB failed: " ++ show e
    Right indexItems -> do
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

  let indexItem = mkIndexItem (postMeta post) (planPostUrl plan)
  let klb = renderKlb [indexItem]
  case klb of
    Left e -> do
      putStrLn ("[Error] render KLB failed: " ++ show e)
    Right klbStr -> do
      appendFile tempIndexItemsKlbPath klbStr

