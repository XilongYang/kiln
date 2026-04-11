module Modules.SearchDB where

import Data.List (intercalate)
import Modules.Index.Item
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.String
import Modules.Utils.Klb

-- ---[ Overview ]------------------------------------------------------------
-- | Search database generator for client-side post search.
--
-- This module converts posts into normalized plain-text records and serializes
-- them into the @searchdb.json@ payload consumed by the frontend.

-- ---[ Public API ]------------------------------------------------------------

-- | Search-document record serialized into @searchdb.json@.
data SearchItem = SearchItem
  { searchItemTitle   :: String
  , searchItemUrl     :: Url
  , searchItemContent :: String
  } deriving (Show, Eq)

instance Klb SearchItem where
  toKlbBlock item = 
    [ ("searchItemTitle", searchItemTitle item)
    , ("searchItemUrl", searchItemUrl item)
    , ("searchItemContent", searchItemContent item)
    ]
  fromKlbBlock block = SearchItem
    { searchItemTitle = getOrError "searchItemTitle"
    , searchItemUrl = getOrError "searchItemUrl"
    , searchItemContent = getOrError "searchItemContent"
    }
    where
      getOrError :: String -> String
      getOrError key =
        case lookup key block of
          Just value -> value
          Nothing    -> error ("Missing value of key: " ++ key)


