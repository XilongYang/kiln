module Modules.SearchDB where

import Data.List (intercalate)
import Modules.Index.Item
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.String
import System.Process (readProcess)

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

-- | Generates and writes the site search database JSON file.
--
-- For each post, markdown is converted to plain text through @pandoc@,
-- normalized into a one-line escaped content string, and then serialized.
genSearchDB :: FilePath -> [Post] -> IO ()
genSearchDB path posts = do
  indexContentPairs <- mapM postToIndexContentPair posts
  let searchDB = mkSearchDB $  map (mkSearchJson . mkSearchItem) indexContentPairs
  writeFile path searchDB
 
-- ---[ Implementation Details ]-----------------------------------------------

-- | Wraps serialized post entries into the final search-db object payload.
mkSearchDB :: [SearchJson] -> SearchJson
mkSearchDB jsons = "{\"posts\": [" ++ intercalate ", " jsons ++ "]}"

-- | Serializes a 'SearchItem' into a flat JSON object string.
--
-- Input content is expected to be pre-escaped by caller-side normalization.
mkSearchJson :: SearchItem -> SearchJson
mkSearchJson item =
  "{\"title\": \"" ++ searchItemTitle item
  ++ "\", \"url\": \"" ++ searchItemUrl item
  ++ "\", \"content\": \"" ++ searchItemContent item
  ++ "\"}"

-- | Converts @(IndexItem, normalized-content)@ into 'SearchItem'.
mkSearchItem :: (IndexItem, String) -> SearchItem
mkSearchItem (item, content) = SearchItem
  { searchItemTitle = title
  , searchItemUrl = url
  , searchItemContent = content
  }
  where
    title = itemTitle item
    url = itemUrl item

-- | Converts one post into index metadata + searchable plaintext content.
--
-- The markdown source file is rendered to plain text by @pandoc@, then escaped
-- for JSON embedding and collapsed into a single whitespace-normalized line.
postToIndexContentPair :: Post -> IO (IndexItem, String)
postToIndexContentPair post = (item,) <$> content
  where
  item = mkIndexItem (postMeta post) "" -- TODO Impl pending
  clear = unwords . words . (replace "\"" "\\\"") . (replace "\\" "\\\\") . (replace "\n" "")
  content = clear <$> readProcess "pandoc"
    [ postBody post -- TODO Impl pending
    , "-t", "plain"
    ]
    ""
