module Modules.SearchDB where

import Data.List (intercalate)
import Modules.IndexItem
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.String
import System.Process (readProcess)

data SearchItem = SearchItem
  { searchItemTitle   :: String
  , searchItemUrl     :: Url
  , searchItemContent :: String
  } deriving (Show, Eq)

genSearchDB :: FilePath -> [Post] -> IO ()
genSearchDB path posts = do
  indexContentPairs <- mapM postToIndexContentPair posts
  let searchDB = mkSearchDB $  map (mkSearchJson . mkSearchItem) indexContentPairs
  writeFile path searchDB
 

mkSearchDB :: [SearchJson] -> SearchJson
mkSearchDB jsons = "{\"posts\": [" ++ intercalate ", " jsons ++ "]}"

mkSearchJson :: SearchItem -> SearchJson
mkSearchJson item =
  "{\"title\": \"" ++ searchItemTitle item
  ++ "\", \"url\": \"" ++ searchItemUrl item
  ++ "\", \"content\": \"" ++ searchItemContent item
  ++ "\"}"

mkSearchItem :: (IndexItem, String) -> SearchItem
mkSearchItem (item, content) = SearchItem
  { searchItemTitle = title
  , searchItemUrl = url
  , searchItemContent = content
  }
  where
    title = itemTitle item
    url = itemUrl item

postToIndexContentPair :: Post -> IO (IndexItem, String)
postToIndexContentPair post = (item,) <$> content
  where
  item = mkIndexItem post
  clear = unwords . words . (replace "\"" "\\\"") . (replace "\\" "\\\\") . (replace "\n" "")
  content = clear <$> readProcess "pandoc"
    [ postSourcePath post
    , "-t", "plain"
    ]
    ""
