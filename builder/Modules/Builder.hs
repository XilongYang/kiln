module Modules.Builder where

import Modules.BuildPlan
import Modules.Post
import Modules.Utils.String

import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import System.IO (writeFile)
import System.Process (callProcess)
import System.FilePath
import System.Directory (createDirectoryIfMissing)

-- Small alias to make HTML-focused helper signatures easier to read.
type Html = String

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
  let postTemplatePath = planPostTemplatePath plan
  let targetHtmlPath = planTargetHtmlPath plan
  writeFile preprocessedPath $ genPreprocessedPostText post
  runPandoc preprocessedPath postTemplatePath targetHtmlPath

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
  , "[[toc]]\n"
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
runPandoc inputPath templatePath outputPath = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  callProcess "pandoc" (mkPandocArgs inputPath templatePath outputPath)

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
  
-- Fills an index template by injecting generated post-list HTML.
genIndexHtml :: [IndexItem] -> Html -> Html
genIndexHtml items template = 
  replace "$posts$" itemsHtml template 
  where
    itemsHtml = unlines 
      [ "<div class=\"post-wrapper\">"
      , yearGroupdHtml items
      , "</div>"
      ]

-- Groups items by year and renders each year section.
yearGroupdHtml :: [IndexItem] -> Html
yearGroupdHtml = unlines . (map singleYearHtml). groupItemsByYear 

-- Renders one year group heading and its post rows.
singleYearHtml :: (String, [IndexItem]) -> Html
singleYearHtml (year, items) = unlines 
  [ "<div class=\"post-year-wrapper\">"
  , "<h3>" ++ year ++ "</h3>"
  ++ itemsToHtml items
  , "</div>"
  ]

-- Renders year-local post rows in reverse chronological order.
--
-- Ordering key is `(month, day)` descending, with month/day represented as
-- zero-padded strings from source metadata.
itemsToHtml :: [IndexItem] -> Html
itemsToHtml = unlines . (map itemToHtml) . 
  reverse .
  (sortOn (\x -> (itemMonth x, itemDate x)))
  where
    itemToHtml item = unlines 
      [ "<div class=\"post-wrapper\">"
      , "<p>" ++ itemDate item 
      ++" <a href=\"" ++ itemUrl item 
      ++ "\">" ++ itemTitle item 
      ++ "</a></p>"
      , "</div>"
      ]
    itemDate item = itemMonth item ++ "-" ++ itemDay item

-- Groups items by year and returns years in descending order.
groupItemsByYear :: [IndexItem] -> [(String, [IndexItem])]
groupItemsByYear = reverse . mergeItemsWithYear . (map itemWithYear)
  where
    itemWithYear item = (itemYear item, item) 

-- Collects `(year, item)` pairs into `(year, [item])`.
mergeItemsWithYear :: [(String, IndexItem)] -> [(String, [IndexItem])]
mergeItemsWithYear pairs = Map.toList $
  Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

