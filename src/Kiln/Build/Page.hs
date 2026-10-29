{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.Page (PostListItem (..), postListItems, absoluteItems, writePage) where

import Data.Aeson (ToJSON (..), encodeFile, object, (.=))
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Ord (Down (..), comparing)
import Kiln.Build.Cache (componentsFingerprint, isGlobalFresh, recordGlobal)
import Kiln.Build.Env
  ( BuildEnv (..)
  , absoluteUrl
  , cachePath
  , logMsg
  , postUrl
  , siteRoot
  , withContext
  , workPath
  )
import Kiln.Build.Markdown (absolutizeUrls)
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (PageConfig (..))
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
-- very first or last post overall (`itemFirst`, e.g. for a feed's own
-- @\<updated\>@, which is the newest post's date; `itemLast`, e.g. for a
-- flat chronological list that wants a separator between posts but not a
-- trailing one).
data PostListItem = PostListItem
  { itemTitle      :: String
  , itemDate       :: String
  , itemMonthDay   :: String
  , itemUrl        :: Maybe String
    -- ^ `Nothing` -- JSON @null@, so a template's @$if(posts.url)$@ sees
    -- it as unset -- when the site renders no standalone post pages at
    -- all, because then there is no page to link to. See
    -- `Kiln.Build.Env.postUrl`.
  , itemTags       :: String
  , itemAbstract   :: Maybe String
  , itemContent    :: String
  , itemYear       :: String
  , itemNewYear    :: Bool
  , itemLastOfYear :: Bool
  , itemFirst      :: Bool
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
      , "first" .= itemFirst i
      , "last" .= itemLast i
      ]

-- | Reshape `entries` into the @posts@ list a page template's
-- @$for(posts)$@ loop sees: newest first, each carrying its own
-- `itemYear`/`itemNewYear`/`itemLastOfYear` so a template can lay out a
-- "grouped by year" list (like the homepage's) purely from
-- `$if(posts.newYear)$`/`$if(posts.lastOfYear)$`, with no help from
-- Haskell beyond this precomputed grouping.
postListItems :: BuildEnv -> [PostEntry] -> [PostListItem]
postListItems env entries = go Nothing (map withYear sorted)
  where
    sorted = sortBy (comparing (Down . postDate)) entries
    withYear e = (e, take 4 (postDate e))
    go _ [] = []
    go prevYear ((e, year) : rest) =
      PostListItem
        { itemTitle = postTitle e
        , itemDate = postDate e
        , itemMonthDay = drop 5 (postDate e)
        , itemUrl = postUrl env (postSlug e)
        , itemTags = postTags e
        , itemAbstract = postAbstract e
        , itemContent = postContent e
        , itemYear = year
        , itemNewYear = prevYear /= Just year
        , itemLastOfYear = nextYear /= Just year
        , itemFirst = isNothing prevYear -- no post preceded this one
        , itemLast = null rest
        }
        : go (Just year) rest
      where
        nextYear = case rest of
          (_, y) : _ -> Just y
          []         -> Nothing

-- | Re-point every URL in `items` at the site's own origin: each post's
-- own `itemUrl`, and the links and media inside its rendered
-- `itemAbstract`/`itemContent` (see
-- `Kiln.Build.Markdown.absolutizeUrls`).
--
-- Applied to the pages that ask for it (`Kiln.Config.pageAbsoluteUrls`),
-- which in practice means the feeds: a feed is read inside an
-- aggregator, so @\/res\/img.png@ -- correct on every page of the site
-- itself -- resolves against the aggregator's domain and the image
-- simply doesn't load. The post's own page is the base every one of
-- those URLs is resolved against, which is why this runs on the item
-- list rather than on the post as it's rendered: it's the page's
-- question, not the post's.
absoluteItems :: BuildEnv -> [PostListItem] -> [PostListItem]
absoluteItems env = map absolutize
  where
    absolutize item =
      item
        { itemUrl = pageUrl
        , itemAbstract = rewrite <$> itemAbstract item
        , itemContent = rewrite (itemContent item)
        }
      where
        pageUrl = absoluteUrl env <$> itemUrl item
        rewrite = absolutizeUrls (absoluteUrl env) pageUrl

-- | Render `page`'s template (found in the build's template copy)
-- through pandoc into `pageOutput page`, filling in its `$webroot$` and
-- `$site-url$` variables and a @posts@ metadata list (every post, newest
-- first, as `postListItems` shapes it -- with every URL made absolute if
-- the page asked for that) a template can lay out itself via
-- `$for(posts)$`, and resolving any `${ component/*() }` partials it
-- references.
--
-- Skipped, reusing the existing output, when neither the template, any
-- component it could reference, the site's webroot or origin, the page's
-- own URL style, kiln's own version, nor the post list itself has
-- changed since the last build. The post
-- list enters the fingerprint whole, so editing one post rebuilds every
-- page -- which is the honest cost of handing every page every post
-- through `$for(posts)$`, since any of them may be laid out from any
-- part of it.
writePage :: BuildEnv -> [PostEntry] -> PageConfig -> IO ()
writePage env entries page = withContext ("while building " ++ outputPath) $ do
  template <- readFile' templatePath
  components <- componentsFingerprint (envTemplateDir env)
  outputExists <- doesFileExist outputPath
  let fingerprint =
        unlines
          [ envTools env
          , envWebroot env
          , siteRoot env
          , show (pageAbsoluteUrls page)
          , template
          , components
          , show entries
          ]
  fresh <- isGlobalFresh fingerprintCachePath fingerprint
  if fresh && outputExists
    then pure ()
    else do
      logMsg ("  " ++ outputPath)
      createDirectoryIfMissing True (takeDirectory outputPath)
      createDirectoryIfMissing True (takeDirectory metadataPath)
      encodeFile metadataPath (object ["posts" .= items])
      writeFile emptyInputPath ""
      callProcess
        "pandoc"
        [ "--quiet"
        , "--template=" ++ templatePath
        , "--variable=webroot=" ++ envWebroot env
        , "--variable=site-url=" ++ siteRoot env
        , "--metadata-file=" ++ metadataPath
        , "--output=" ++ outputPath
        , emptyInputPath
        ]
      recordGlobal fingerprintCachePath fingerprint
  where
    outputPath = pageOutput page
    templatePath = envTemplateDir env </> pageTemplate page
    items
      | pageAbsoluteUrls page = absoluteItems env (postListItems env entries)
      | otherwise             = postListItems env entries
    -- `outputPath` is already a safe, unique key (see `PageConfig`'s own
    -- doc comment) -- mirror its own directory structure under the work
    -- dir / cache dir rather than inventing a separate flat name, so
    -- nested outputs (e.g. "about/index.html") can't collide with an
    -- unrelated page that merely shares a basename.
    emptyInputPath = workPath env ["page", outputPath ++ ".md"]
    metadataPath = workPath env ["page", outputPath ++ "-metadata.json"]
    fingerprintCachePath = cachePath env ["pages", outputPath ++ ".cache"]
