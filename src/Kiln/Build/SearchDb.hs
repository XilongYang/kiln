{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.SearchDb (writeSearchDb) where

import Data.Aeson (ToJSON (..), encodeFile, object, (.=))
import Kiln.Build.Env (BuildEnv (..), postUrl)
import Kiln.Build.PostEntry (PostEntry (..))

data SearchEntry = SearchEntry
  { searchTitle   :: String
  , searchUrl     :: Maybe String
  , searchContent :: String
  }

instance ToJSON SearchEntry where
  toJSON e =
    object
      [ "title" .= searchTitle e
      , "url" .= searchUrl e
      , "content" .= searchContent e
      ]

-- | Write the search index -- title, url and plain-text content per post
-- -- or nothing at all for a site with no search to feed.
--
-- Kept in `Kiln.Build.Post.renderPosts`' order, which is sorted by
-- source file name, so rebuilding an unchanged site rewrites a
-- byte-identical file.
writeSearchDb :: BuildEnv -> [(PostEntry, String)] -> IO ()
writeSearchDb env entries =
  case envSearchDb env of
    Nothing     -> pure ()
    Just dbPath -> encodeFile dbPath (map toSearchEntry entries)
  where
    toSearchEntry (entry, content) =
      SearchEntry
        { searchTitle = postTitle entry
        , searchUrl = postUrl env (postSlug entry)
        , searchContent = content
        }
