module Modules.IndexItem where

import Modules.Config
import Modules.Post
import Modules.TypeAlias

-- One row shown in the homepage index list.
data IndexItem = IndexItem
  { itemTitle            :: String
  -- Date components are kept as zero-padded strings for lexical sorting.
  , itemYear             :: String
  , itemMonth            :: String
  , itemDay              :: String
  -- Absolute or site-root-relative URL to the generated post page.
  , itemUrl              :: Url
  } deriving (Show, Eq)

mkIndexItem :: Post -> IndexItem
mkIndexItem post = IndexItem 
  { itemTitle = metaTitle (postMeta post)
  , itemYear = (reverse . (drop 6) . reverse) date
  , itemMonth = (reverse . (drop 3) . reverse . (drop 5)) date
  , itemDay = drop 8 date
  , itemUrl = webPostPath ++ (postName post) ++ ".html"
  }
  where
    date = metaDate (postMeta post)

