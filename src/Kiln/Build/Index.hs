module Kiln.Build.Index (renderPostsList, writeIndex) where

import Data.List (groupBy, sortBy)
import Data.Ord (Down (..), comparing)
import Kiln.Build.Cache (componentsFingerprint, isGlobalFresh, recordGlobal)
import Kiln.Build.PostEntry (PostEntry (..), postUrl)
import Kiln.Config (KilnConfig (..), OutPaths (..), PathConfig (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | Render the index.html template at `dir </> "index.html"` through
-- pandoc into `config`'s index output path, filling in its `$webroot$`
-- and `$posts$` variables and resolving any `${ component/*() }`
-- partials it references. Skipped, reusing the existing output, when
-- neither the template, any component it could reference, the site's
-- webroot, nor the post list itself has changed since the last build.
writeIndex :: KilnConfig -> FilePath -> [PostEntry] -> IO ()
writeIndex config dir entries = do
  template <- readFile' indexTemplatePath
  components <- componentsFingerprint dir
  outputExists <- doesFileExist indexPath
  let postsHtml = renderPostsList webroot entries
      fingerprint = webroot ++ "\n" ++ template ++ "\n" ++ components ++ "\n" ++ postsHtml
  fresh <- isGlobalFresh fingerprintCachePath fingerprint
  if fresh && outputExists
    then pure ()
    else do
      writeFile emptyInputPath ""
      callProcess
        "pandoc"
        [ "--quiet"
        , "--template=" ++ indexTemplatePath
        , "--variable=webroot=" ++ webroot
        , "--variable=posts=" ++ postsHtml
        , "--output=" ++ indexPath
        , emptyInputPath
        ]
      recordGlobal fingerprintCachePath fingerprint
  where
    outPaths = pathOut (configPath config)
    webroot = configWebroot config
    indexPath = outIndex outPaths
    indexTemplatePath = dir </> "index.html"
    emptyInputPath = dir </> "index.md"
    fingerprintCachePath = outCache outPaths </> "index-global"

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
