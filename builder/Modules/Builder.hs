module Modules.Builder where

import Modules.BuildPlan
import Modules.Post
import Modules.Utils.String

import Data.List (isPrefixOf)
import System.IO (writeFile)
import System.Process (callProcess)

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
  return ()

-- Writes preprocessed markdown for one post build plan.
--
-- The generated markdown is consumed by the downstream renderer.
buildPostWithPlan :: PostBuildPlan -> IO()
buildPostWithPlan = buildPostWithPlanWith runPandoc

-- Variant of `buildPostWithPlan` with injectable pandoc runner for UT.
--
-- This keeps production behavior identical while allowing tests to validate
-- generated preprocess content and pandoc invocation parameters without
-- depending on an actual pandoc binary/runtime.
buildPostWithPlanWith :: (FilePath -> FilePath -> FilePath -> IO ()) -> PostBuildPlan -> IO ()
buildPostWithPlanWith pandocRunner plan = do
  let post = planPost plan
  let preprocessedPath = planPreprocessedPath plan
  let postTemplatePath = planPostTemplatePath plan
  let targetHtmlPath = planTargetHtmlPath plan
  writeFile preprocessedPath $ genPreprocessedPostText post
  pandocRunner preprocessedPath postTemplatePath targetHtmlPath

-- Builds the preprocessed markdown payload used before HTML rendering.
--
-- Layout:
-- 1) serialized front matter
-- 2) wrapped abstract block
-- 3) table-of-contents marker
-- 4) body markdown with rewritten fenced-code marks
genPreprocessedPostText :: Post -> String
genPreprocessedPostText post = unlines
  [ rawMeta
  , abstract
  , "[[toc]]"
  , rewriteLanguageMarks $ postContent post
  ]
  where
    rawMeta = revertMeta $ postMeta post
    abstract = unlines
      [ "<div class=\"abstract\">"
      , postAbstract post
      , "</div>"
      ]

-- Rewrites all lines so fenced-code language marks are normalized.
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks = 
  unlines . map rewriteLanguageMarkLine . lines

-- Rewrites one markdown line if it is a fenced-code opening with language tag.
--
-- Example:
-- - input:  ```haskell
-- - output: ``` {.language-haskell .line-numbers .match-braces}
rewriteLanguageMarkLine :: String -> String
rewriteLanguageMarkLine line
  | not $  "```" `isPrefixOf` line = line
  | mark == "" = line
  | otherwise = replace "[mark]" mark "``` {.language-[mark] .line-numbers .match-braces}"
  where
    mark = trim $ drop 3 line

-- Builds pandoc CLI arguments for markdown-to-html conversion.
--
-- Parameter order:
-- 1) input markdown path
-- 2) template html path
-- 3) output html path
mkPandocArgs :: FilePath -> FilePath -> FilePath -> [String]
mkPandocArgs inputPath templatePath outputPath =
  [ inputPath
  , "-o", outputPath
  , "--template=" ++ templatePath
  , "--no-highlight"
  , "--mathjax"
  , "--wrap=none"
  ]

-- Executes pandoc with standardized flags used by the builder.
runPandoc :: FilePath -> FilePath -> FilePath -> IO ()
runPandoc inputPath templatePath outputPath =
  callProcess "pandoc" (mkPandocArgs inputPath templatePath outputPath)
