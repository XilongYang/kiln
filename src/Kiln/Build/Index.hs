module Kiln.Build.Index (renderPostsList, writeIndex) where

import Data.List (groupBy, sortBy)
import Data.Ord (Down (..), comparing)
import Kiln.Build.PostEntry (PostEntry (..), postUrl)
import Kiln.Build.Template (replaceAll)
import Kiln.Config (KilnConfig (..), OutPaths (..), PathConfig (..))
import System.FilePath ((</>))
import System.IO (readFile')

-- | Fill in the `$posts$` and `$webroot$` placeholders of the (already
-- component-substituted) index.html template at `dir </> "index.html"`
-- and write the result to `config`'s index output path.
writeIndex :: KilnConfig -> FilePath -> [PostEntry] -> IO ()
writeIndex config dir entries = do
  content <- readFile' indexTemplatePath
  let withPosts = replaceAll "$posts$" (renderPostsList webroot entries) content
  writeFile indexPath (replaceAll "$webroot$" webroot withPosts)
  where
    outPaths = pathOut (configPath config)
    webroot = configWebroot config
    indexPath = outIndex outPaths
    indexTemplatePath = dir </> "index.html"

-- | Render the posts list markup that fills the @$posts$@ placeholder in
-- index.html: entries grouped by year (newest year first), newest post
-- first within each year. `webroot` is prepended to every post link.
renderPostsList :: String -> [PostEntry] -> String
renderPostsList webroot entries =
  unlines $
    concat
      [ ["<div class=\"post-wrapper\">"]
      , concatMap (renderYear webroot) (groupByYearDesc entries)
      , ["</div>"]
      ]

groupByYearDesc :: [PostEntry] -> [(String, [PostEntry])]
groupByYearDesc entries = [(yearOf e, grp) | grp@(e : _) <- grouped]
  where
    sorted = sortBy (comparing (Down . postDate)) entries
    grouped = groupBy (\a b -> yearOf a == yearOf b) sorted
    yearOf = take 4 . postDate

renderYear :: String -> (String, [PostEntry]) -> [String]
renderYear webroot (year, posts) =
  concat
    [ [ indent 1 "<div class=\"post-year-wrapper\" style=\"display: block;\">"
      , indent 2 ("<h3>" ++ year ++ "</h3>")
      ]
    , concatMap (renderPostEntry webroot) posts
    , [indent 1 "</div>"]
    ]

renderPostEntry :: String -> PostEntry -> [String]
renderPostEntry webroot e =
  [ indent 2 "<div class=\"post-wrapper\" style=\"display: block;\">"
  , indent 3 ("<p>" ++ monthDay (postDate e) ++ " <a href=\"" ++ postUrl webroot e ++ "\">" ++ postTitle e ++ "</a></p>")
  , indent 2 "</div>"
  ]
  where
    monthDay = drop 5

indent :: Int -> String -> String
indent level = (replicate (level * 4) ' ' ++)
