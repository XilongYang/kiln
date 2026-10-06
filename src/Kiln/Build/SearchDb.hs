{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.SearchDb (writeSearchDb) where

import Data.Aeson (ToJSON (..), encodeFile, object, (.=))
import Kiln.Build.PostEntry (PostEntry (..), postUrl)
import Kiln.Config (KilnConfig (..), OutPaths (..), PathConfig (..))

data SearchEntry = SearchEntry
  { searchTitle   :: String
  , searchUrl     :: String
  , searchContent :: String
  }

instance ToJSON SearchEntry where
  toJSON e =
    object
      [ "title" .= searchTitle e
      , "url" .= searchUrl e
      , "content" .= searchContent e
      ]

-- | Build the searchdb.json array (title/url/content per post) from each
-- post's `PostEntry` paired with its plain-text content, and write it to
-- `config`'s search-db output path.
writeSearchDb :: KilnConfig -> [(PostEntry, String)] -> IO ()
writeSearchDb config entries = encodeFile dbPath (map toSearchEntry entries)
  where
    outPaths = pathOut (configPath config)
    dbPath = outSearchDb outPaths
    webroot = configWebroot config
    toSearchEntry (entry, content) =
      SearchEntry
        { searchTitle = postTitle entry
        , searchUrl = postUrl webroot entry
        , searchContent = content
        }
