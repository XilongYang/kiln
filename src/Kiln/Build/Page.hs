{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.Page (PostListItem (..), postListItems, writePage) where

import Data.Aeson (ToJSON (..), encodeFile, object, (.=))
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Kiln.Build.Cache (componentsFingerprint, isGlobalFresh, recordGlobal)
import Kiln.Build.PostEntry (PostEntry (..), postUrl)
import Kiln.Config (KilnConfig (..), OptConfig (..), PageConfig (..), TargetConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | One post, reshaped for a page template's @$for(posts)$@ loop: every
-- field `PostEntry` has, plus display-only fields a template can't
-- compute on its own -- the post's absolute `itemUrl`, its date split
-- into year/month-day for display, whether it's the first
-- (`itemNewYear`) or last (`itemLastOfYear`) post of its year in
-- `postListItems`' sort order (so a template can open/close a per-year
-- wrapper without any stateful looping of its own), and whether it's the
-- very last post overall (`itemLast`, e.g. for a flat chronological list
-- that wants a separator between posts but not a trailing one).
data PostListItem = PostListItem
  { itemTitle      :: String
  , itemDate       :: String
  , itemMonthDay   :: String
  , itemUrl        :: String
  , itemTags       :: String
  , itemAbstract   :: Maybe String
  , itemContent    :: String
  , itemYear       :: String
  , itemNewYear    :: Bool
  , itemLastOfYear :: Bool
  , itemLast       :: Bool
  } deriving (Show, Eq)

instance ToJSON PostListItem where
  toJSON i =
    object
      [ "title" .= itemTitle i
      , "date" .= itemDate i
      , "monthDay" .= itemMonthDay i
      , "url" .= itemUrl i
      , "tags" .= itemTags i
      , "abstract" .= itemAbstract i
      , "content" .= itemContent i
      , "year" .= itemYear i
      , "newYear" .= itemNewYear i
      , "lastOfYear" .= itemLastOfYear i
      , "last" .= itemLast i
      ]

-- | Reshape `entries` into the @posts@ list a page template's
-- @$for(posts)$@ loop sees: newest first, each carrying its own
-- `itemYear`/`itemNewYear`/`itemLastOfYear` so a template can lay out a
-- "grouped by year" list (like the homepage's) purely from
-- `$if(posts.newYear)$`/`$if(posts.lastOfYear)$`, with no help from
-- Haskell beyond this precomputed grouping.
postListItems :: String -> [PostEntry] -> [PostListItem]
postListItems webroot entries = go Nothing (map withYear sorted)
  where
    sorted = sortBy (comparing (Down . postDate)) entries
    withYear e = (e, take 4 (postDate e))
    go _ [] = []
    go prevYear ((e, year) : rest) =
      PostListItem
        { itemTitle = postTitle e
        , itemDate = postDate e
        , itemMonthDay = drop 5 (postDate e)
        , itemUrl = postUrl webroot e
        , itemTags = postTags e
        , itemAbstract = postAbstract e
        , itemContent = postContent e
        , itemYear = year
        , itemNewYear = prevYear /= Just year
        , itemLastOfYear = nextYear /= Just year
        , itemLast = null rest
        }
        : go (Just year) rest
      where
        nextYear = case rest of
          (_, y) : _ -> Just y
          []         -> Nothing

-- | Render `page`'s template (found at `dir </> pageTemplate page`)
-- through pandoc into `pageOutput page`, filling in its `$webroot$`
-- variable and a @posts@ metadata list (every post, newest first, as
-- `postListItems` shapes it) a template can lay out itself via
-- `$for(posts)$`, and resolving any `${ component/*() }` partials it
-- references. Skipped, reusing the existing output, when neither the
-- template, any component it could reference, the site's webroot, nor
-- the post list itself has changed since the last build.
writePage :: KilnConfig -> FilePath -> [PostEntry] -> PageConfig -> IO ()
writePage config dir entries page = do
  template <- readFile' templatePath
  components <- componentsFingerprint dir
  outputExists <- doesFileExist outputPath
  let fingerprint = webroot ++ "\n" ++ template ++ "\n" ++ components ++ "\n" ++ show entries
  fresh <- isGlobalFresh fingerprintCachePath fingerprint
  if fresh && outputExists
    then pure ()
    else do
      createDirectoryIfMissing True (takeDirectory outputPath)
      createDirectoryIfMissing True (takeDirectory metadataPath)
      encodeFile metadataPath (object ["posts" .= postListItems webroot entries])
      writeFile emptyInputPath ""
      callProcess
        "pandoc"
        [ "--quiet"
        , "--template=" ++ templatePath
        , "--variable=webroot=" ++ webroot
        , "--metadata-file=" ++ metadataPath
        , "--output=" ++ outputPath
        , emptyInputPath
        ]
      recordGlobal fingerprintCachePath fingerprint
  where
    webroot = optWebroot (configOpt config)
    outputPath = pageOutput page
    templatePath = dir </> pageTemplate page
    -- `outputPath` is already a safe, unique key (see `PageConfig`'s own
    -- doc comment) -- mirror its own directory structure under the temp
    -- dir / cache dir rather than inventing a separate flat name, so
    -- nested outputs (e.g. "about/index.html") can't collide with an
    -- unrelated page that merely shares a basename.
    emptyInputPath = dir </> "pages" </> (outputPath ++ ".md")
    metadataPath = dir </> "pages" </> (outputPath ++ "-metadata.json")
    fingerprintCachePath = targetCacheDir (configTarget config) </> "pages" </> (outputPath ++ ".cache")
