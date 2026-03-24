module Modules.IndexItem where

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
