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

-- | Build the searchdb.json array (title/url/content per post) from each
-- post's `PostEntry` paired with its plain-text content, and write it to
-- `env`'s search-db output path -- or do nothing if the site has no
-- search feature to feed (`envSearchDb` is JSON @null@).
--
-- The array is in the order `Kiln.Build.Post.renderPosts` returns, which
-- is sorted by source file name, so rebuilding an unchanged site rewrites
-- a byte-identical file.
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
