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

-- | One post, reshaped for a template's @$for(posts)$@ loop: every
-- `PostEntry` field, plus what a template can't compute on its own --
-- its URL, its date split for display, and its position in the list, so
-- a per-year wrapper (`itemNewYear`\/`itemLastOfYear`), a feed's own
-- @\<updated\>@ (`itemFirst`) or a separator between posts but not after
-- the last (`itemLast`) need no stateful looping.
data PostListItem = PostListItem
  { itemTitle      :: String
  , itemDate       :: String
  , itemMonthDay   :: String
  , itemUrl        :: Maybe String
    -- ^ `Nothing`, i.e. JSON @null@ for a template's @$if(posts.url)$@,
    -- when the site renders no post pages and there is nothing to link
    -- to. See `Kiln.Build.Env.postUrl`.
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

-- | Reshape `entries` into the @posts@ list a template sees: newest
-- first, the year grouping precomputed so a "grouped by year" layout is
-- just @$if(posts.newYear)$@ and @$if(posts.lastOfYear)$@.
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

-- | Re-point every URL in `items` at the site's origin -- each
-- `itemUrl`, and the links and media inside the rendered
-- `itemAbstract`\/`itemContent` (see
-- `Kiln.Build.Markdown.absolutizeUrls`).
--
-- For the pages that ask (`Kiln.Config.pageAbsoluteUrls`), meaning the
-- feeds: read in an aggregator, @\/res\/img.png@ resolves against the
-- aggregator's domain and the image doesn't load. Run on the item list
-- rather than on the post as it renders, because it is the page's
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

-- | Render `page`'s template into `pageOutput page`, filling in its
-- @$webroot$@ and @$site-url$@ variables, the @posts@ list
-- `postListItems` shapes, and any @${ component\/*() }@ partials.
--
-- Served from the existing output when neither the template, a component
-- it could reference, the webroot or origin, the page's URL style,
-- kiln's version nor the post list has changed. The post list enters the
-- fingerprint whole, so editing one post rebuilds every page -- the cost
-- of handing every page every post, any part of which it may lay out.
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
        -- A metadata value is read as markdown, but every one here is
        -- already rendered HTML or a plain string: without these, the
        -- @--@ in a code span, a url or a tag comes out an en dash, and
        -- markup inside a post's own HTML gets found a second time.
        , "--from=markdown-smart-markdown_in_html_blocks"
        , "--wrap=none"
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
    -- `outputPath` is already a unique key (see `PageConfig`), so mirror
    -- its structure rather than inventing a flat name: "about/index.html"
    -- must not collide with a page that merely shares a basename.
    emptyInputPath = workPath env ["page", outputPath ++ ".md"]
    metadataPath = workPath env ["page", outputPath ++ "-metadata.json"]
    fingerprintCachePath = cachePath env ["pages", outputPath ++ ".cache"]
