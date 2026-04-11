module Modules.Builder (executeBuildPlan) where

import Modules.BuildPlan
import Modules.BuildJudger
import Modules.Index.Render
import Modules.Post.Preprocess
import Modules.Post.Parse
import Modules.Pandoc
import Modules.Toc
import Modules.Index.Item (mkIndexItem)
import Modules.Post (Post(..), PostMeta (metaTitle))
import Modules.Utils.Klb
import Modules.Config (metaArtifactsPath, charsetArtifactsPath)
import Modules.TypeAlias (Url)
import Modules.SearchDB
import Modules.FontSubset

import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Build-plan executor for post pages and homepage index generation.
--
-- This module is the runtime orchestrator of the site builder:
-- - receives typed plans from 'Modules.BuildPlan'
-- - asks 'Modules.BuildJudger' whether each plan should run
-- - executes post/index build workflows
-- - writes per-post cache artifacts consumed by later aggregation steps
--
-- High-level responsibilities:
-- - gate execution through 'shouldBuild'
-- - dispatch to post/index specific executors
-- - coordinate parse/preprocess/render/write stages
-- - create target/artifact directories on demand before writes
-- - materialize metadata/search/charset artifacts
-- - print recoverable build errors to stdout

-- ---[ Public API ]------------------------------------------------------------

-- | Error type for post-build sub-steps.
--
-- Currently kept as module-local documentation of failure classes.
-- The runtime still reports errors by logging instead of returning this type.
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

-- | Builds @index.html@ from cached per-post metadata artifacts.
--
-- Steps:
-- - read per-post metadata KLB artifacts from cache
-- - parse KLB into in-memory index items
-- - read index template HTML
-- - render final index HTML and write to target path
-- - emit index charset artifact used by font subsetting
--
-- On KLB parse failure, logs an error and skips writing output.
buildIndexWithPlan :: IndexBuildPlan -> IO ()
buildIndexWithPlan plan = do
  let indexHtmlPath = planIndexHtmlPath plan
  putStrLn ("[Building] index: " ++ indexHtmlPath)
  metaFileNames <- listDirectory metaArtifactsPath 
  let metaPaths = map (\f -> metaArtifactsPath  </> f) $ filter (\f -> takeExtension f == ".klb") metaFileNames
  indexItemsKlbs <- traverse readFile metaPaths
  let eitherIndexItems = parseKlb (concat indexItemsKlbs)
  case eitherIndexItems of
    Left e -> do
      putStrLn $ "[Error] parse KLB failed: " ++ show e
    Right indexItems -> do
      let indexTemplatePath = planIndexTemplatePath plan
      indexTemplateHtml <- readFile indexTemplatePath
      let indexHtml = renderIndex indexItems indexTemplateHtml 
      writeFile indexHtmlPath indexHtml
      writeCharset (charsetArtifactsPath </> "index.txt") indexHtmlPath

-- | Builds one post page and all per-post artifacts from a post plan.
--
-- Pipeline:
-- - parse source markdown into structured 'Post'
-- - preprocess parsed post into intermediate markdown
-- - render HTML via pandoc and post template
-- - inject TOC into rendered HTML
-- - write per-post metadata/search-item/charset artifacts for downstream steps
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
  putStrLn ("[Building] post: " ++ sourcePath)
  case post of
    Left e -> do
      putStrLn ("[Error] parse post failed: " ++ show e ++ " : " ++ sourcePath)
    Right post -> do
      writeFile preprocessedPath $ preprocessPost post
      runPandoc preprocessedPath postTemplatePath builtHtmlPath
      builtHtml <- readFile builtHtmlPath
      createDirectoryIfMissing True (takeDirectory targetHtmlPath)
      writeFile targetHtmlPath $ injectToc builtHtml
      writePostMetaKlb (planPostMetaPath plan) (postMeta post) (planPostUrl plan)
      writePostSearchItemKlb (planPostSearchItemPath plan) (postMeta post) (planPostUrl plan) preprocessedPath
      writeCharset (planPostCharsetPath plan) targetHtmlPath

-- | Writes one post's index metadata artifact (@IndexItem@ in KLB format).
--
-- On KLB render failure, logs an error and does not write output.
writePostMetaKlb :: FilePath -> PostMeta -> Url -> IO ()
writePostMetaKlb path meta url = do
  let indexItem = mkIndexItem meta url
  writeKlbOrError path indexItem

-- | Writes one post's search artifact as a 'SearchItem' KLB block.
--
-- Content is obtained by converting preprocessed markdown to plaintext with
-- pandoc, then normalized by KLB rendering.
writePostSearchItemKlb :: FilePath -> PostMeta -> Url -> FilePath -> IO ()
writePostSearchItemKlb path meta url preprocessedPath = do
  let title = metaTitle meta
  plaintext <- renderMarkdownToPlaintext preprocessedPath
  writeKlbOrError path (SearchItem title url plaintext)

-- | Writes charset artifact for one generated HTML file.
--
-- The artifact contains unique characters only and is consumed by
-- 'Modules.FontSubset.genFontSubset'.
writeCharset :: FilePath -> FilePath -> IO ()
writeCharset path htmlPath = do
  html <- readFile htmlPath
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (mkFontSet html)

-- | Generic helper that renders one KLB record and writes it to a file.
--
-- Rendering failures are logged and do not throw.
writeKlbOrError :: Klb a => FilePath -> a -> IO ()
writeKlbOrError path item = do
  let klb = renderKlb [item]
  case klb of
    Left e -> do
      putStrLn ("[Error] render KLB failed: " ++ show e ++ " : " ++ path)
    Right klbStr -> do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path klbStr
