module Modules.SearchDB where

import Modules.TypeAlias
import Modules.Utils.Klb

-- ---[ Overview ]------------------------------------------------------------
-- | Search-item artifact model shared by build and indexing steps.
--
-- The builder writes one KLB block per post and main concatenates those blocks
-- into @searchdb.json@ consumed by the frontend search script.

-- ---[ Public API ]------------------------------------------------------------

-- | Search-document record serialized as KLB and concatenated into @searchdb.json@.
--
-- Each post contributes exactly one record.
data SearchItem = SearchItem
  { searchItemTitle   :: String
  , searchItemUrl     :: Url
  , searchItemContent :: String
  } deriving (Show, Eq)

instance Klb SearchItem where
  -- | Encodes one search item into a KLB block with stable key names.
  toKlbBlock item = 
    [ ("searchItemTitle", searchItemTitle item)
    , ("searchItemUrl", searchItemUrl item)
    , ("searchItemContent", searchItemContent item)
    ]
  -- | Decodes one KLB block back to a search item.
  --
  -- Missing required keys raise an error because this is used internally on
  -- builder-generated artifacts where shape is expected to be stable.
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
