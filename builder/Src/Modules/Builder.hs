module Modules.Builder (executeBuildPlan) where

import Modules.BuildPlan
import Modules.BuildJudger
import Modules.Index.Render
import Modules.Post.Preprocess
import Modules.Post.Parse
import Modules.Pandoc
import Modules.Toc
import Modules.Index.Item (mkIndexItem)
import Modules.Post (Post(postMeta), PostMeta)
import Modules.Utils.Klb
import Modules.Config (tempIndexItemsKlbPath)
import Modules.TypeAlias (Url)

-- ---[ Overview ]------------------------------------------------------------
-- | Build-plan executor for post and index generation.
--
-- This module is the orchestration layer of the build pipeline.
-- It receives typed 'BuildPlan' values, checks whether a rebuild is needed,
-- then runs concrete IO workflows for post pages or index page generation.
--
-- High-level responsibilities:
-- - gate execution through 'shouldBuild'
-- - dispatch to post/index specific executors
-- - coordinate parsing, rendering, and output writes
-- - print recoverable build errors to stdout

-- ---[ Public API ]------------------------------------------------------------

-- | Error type for post-build sub-steps.
--
-- This captures structured failures produced by parser and KLB rendering
-- components. The current build runner logs errors directly and does not
-- return this type to callers.
data BuildPostError
  = BuildPostParseError ParsePostError
  | BuildPostRenderKlbError KlbRenderError
  deriving (Show, Eq)

-- | Executes a build plan when it is marked as needing rebuild.
--
-- Behavior:
-- - if 'shouldBuild' is 'False', this is a no-op
-- - if 'shouldBuild' is 'True', delegates to 'realExecuteBuildPlan'
executeBuildPlan :: BuildPlan -> IO ()
executeBuildPlan plan = do
  isShouldBuild <- shouldBuild plan
  if not isShouldBuild then return () else do
    realExecuteBuildPlan plan    

-- ---[ Implementation Details ]-----------------------------------------------

-- | Dispatches concrete build action by 'BuildPlan' constructor.
realExecuteBuildPlan :: BuildPlan -> IO ()
realExecuteBuildPlan (BuildPostPlan plan) = do
  buildPostWithPlan plan
realExecuteBuildPlan (BuildIndexPlan plan) = do
  buildIndexWithPlan plan

-- | Builds @index.html@ from serialized index items and template HTML.
--
-- Steps:
-- - read temporary KLB index-items file
-- - parse KLB into in-memory index items
-- - read index template HTML
-- - render final index HTML and write to target path
--
-- On KLB parse failure, logs an error and skips writing output.
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
-- - parse source markdown into structured 'Post'
-- - preprocess parsed post into intermediate markdown
-- - render HTML via pandoc and post template
-- - inject TOC into rendered HTML
-- - write final HTML and append one index-item KLB record
--
-- On parse failure, logs an error and skips remaining steps for this post.
buildPostWithPlan :: PostBuildPlan -> IO ()
buildPostWithPlan plan = do
  let preprocessedPath = planPreprocessedPath plan
  let builtHtmlPath = planBuiltHtmlPath plan
  let postTemplatePath = planPostTemplatePath plan
  let targetHtmlPath = planTargetHtmlPath plan
  let sourcePath = planPostSourcePath plan
  post <- parsePost sourcePath
  case post of
    Left e -> do
      putStrLn ("[Error] parse post failed: " ++ show e ++ " : " ++ sourcePath)
    Right post -> do
      writeFile preprocessedPath $ preprocessPost post
      runPandoc preprocessedPath postTemplatePath builtHtmlPath
      builtHtml <- readFile builtHtmlPath
      writeFile targetHtmlPath $ injectToc builtHtml
      writePostKlb (postMeta post) (planPostUrl plan)

-- | Appends one post's metadata as KLB index item into temp index file.
--
-- On KLB render failure, logs an error and does not append anything.
writePostKlb :: PostMeta -> Url -> IO ()
writePostKlb meta url = do
  let indexItem = mkIndexItem meta url
  let klb = renderKlb [indexItem]
  case klb of
    Left e -> do
      putStrLn ("[Error] render KLB failed: " ++ show e)
    Right klbStr -> do
      appendFile tempIndexItemsKlbPath klbStr
