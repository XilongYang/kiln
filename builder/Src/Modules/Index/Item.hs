module Modules.Index.Item (IndexItem (..), mkIndexItem) where

import Modules.Config
import Modules.Post
import Modules.TypeAlias

-- ---[ Overview ]------------------------------------------------------------
-- | Index item model and constructor helpers.
--
-- This module defines the lightweight row data rendered on the homepage index
-- and provides conversion from parsed 'Post' values.

-- ---[ Public API ]------------------------------------------------------------

-- | One row rendered in the homepage post list.
data IndexItem = IndexItem
  { itemTitle            :: String -- ^ Display title.
  , itemYear             :: String -- ^ Year component (YYYY).
  , itemMonth            :: String -- ^ Month component (MM).
  , itemDay              :: String -- ^ Day component (DD).
  , itemUrl              :: Url    -- ^ Post URL.
  } deriving (Show, Eq)

-- | Converts a parsed post into an index item.
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

