module Modules.Index.Item (IndexItem (..), mkIndexItem) where

import Modules.Config
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.Klb

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

instance Klb IndexItem where
  toKlbBlock item = 
    [ ("itemTitle", itemTitle item)
    , ("itemYear", itemYear item)
    , ("itemMonth", itemMonth item)
    , ("itemDay", itemDay item)
    , ("itemUrl", itemUrl item)
    ]
  fromKlbBlock block = IndexItem 
    { itemTitle = getOrError "itemTitle"
    , itemYear  = getOrError "itemYear"
    , itemMonth = getOrError "itemMonth"
    , itemDay   = getOrError "itemDay"
    , itemUrl   = getOrError "itemUrl"
    }
    where
      getOrError :: String -> String
      getOrError key =
        case lookup key block of
          Just value -> value
          Nothing    -> error ("Missing value of key: " ++ key)

-- | Converts a parsed post into an index item.
mkIndexItem :: PostMeta -> Url -> IndexItem
mkIndexItem meta url = IndexItem 
  { itemTitle = metaTitle meta
  , itemYear = (reverse . (drop 6) . reverse) date
  , itemMonth = (reverse . (drop 3) . reverse . (drop 5)) date
  , itemDay = drop 8 date
  , itemUrl = url
  }
  where
    date = metaDate meta

