module Kiln.Build.Page (renderPostsList, writePage) where

import Data.List (groupBy, sortBy)
import Data.Ord (Down (..), comparing)
import Kiln.Build.Cache (componentsFingerprint, isGlobalFresh, recordGlobal)
import Kiln.Build.PostEntry (PostEntry (..), postUrl)
import Kiln.Config (KilnConfig (..), OutPaths (..), PageConfig (..), PathConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | Render `page`'s template (found at `dir </> pageTemplate page`)
-- through pandoc into `pageOutput page`, filling in its `$webroot$` and
-- `$posts$` variables and resolving any `${ component/*() }` partials
-- it references. Skipped, reusing the existing output, when neither the
-- template, any component it could reference, the site's webroot, nor
-- the post list itself has changed since the last build.
writePage :: KilnConfig -> FilePath -> [PostEntry] -> PageConfig -> IO ()
writePage config dir entries page = do
  template <- readFile' templatePath
  components <- componentsFingerprint dir
  outputExists <- doesFileExist outputPath
  let postsHtml = renderPostsList webroot entries
      fingerprint = webroot ++ "\n" ++ template ++ "\n" ++ components ++ "\n" ++ postsHtml
  fresh <- isGlobalFresh fingerprintCachePath fingerprint
  if fresh && outputExists
    then pure ()
    else do
      createDirectoryIfMissing True (takeDirectory outputPath)
      writeFile emptyInputPath ""
      callProcess
        "pandoc"
        [ "--quiet"
        , "--template=" ++ templatePath
        , "--variable=webroot=" ++ webroot
        , "--variable=posts=" ++ postsHtml
        , "--output=" ++ outputPath
        , emptyInputPath
        ]
      recordGlobal fingerprintCachePath fingerprint
  where
    outPaths = pathOut (configPath config)
    webroot = configWebroot config
    outputPath = pageOutput page
    templatePath = dir </> pageTemplate page
    emptyInputPath = dir </> (pageName page ++ ".md")
    fingerprintCachePath = outCache outPaths </> ("page-" ++ pageName page ++ "-global")

-- | Render the posts list markup that fills the @$posts$@ placeholder in
-- a page's template: entries grouped by year (newest year first),
-- newest post first within each year. `webroot` is prepended to every
-- post link.
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
