{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.PostEntry (PostEntry (..), itemJsonTemplate, loadPostEntry, postUrl) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))

data PostEntry = PostEntry
  { postTitle :: String
  , postDate  :: String -- ^ "YYYY-MM-DD"
  , postSlug  :: String
  }

instance FromJSON PostEntry where
  parseJSON = withObject "post-entry" $ \o ->
    PostEntry <$> o .: "title" <*> o .: "date" <*> o .: "slug"

-- | pandoc template text that serializes a post's title/date/slug as the
-- JSON the `FromJSON` instance above expects back.
itemJsonTemplate :: String
itemJsonTemplate = "{\"title\": \"$title$\", \"date\": \"$date$\", \"slug\": \"$slug$\"}"

-- | Parse a JSON file produced by pandoc from `itemJsonTemplate` into a
-- `PostEntry`.
loadPostEntry :: FilePath -> IO PostEntry
loadPostEntry jsonPath = either fail pure =<< eitherDecodeFileStrict jsonPath

-- | The site-relative URL a post is published at.
postUrl :: String -> PostEntry -> String
postUrl webroot e = webroot ++ "post/" ++ postSlug e ++ ".html"
