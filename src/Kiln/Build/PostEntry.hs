{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.PostEntry
  ( PostEntry (..)
  , PostFrontmatter (..)
  , frontmatterTemplate
  , loadFrontmatter
  , loadPostEntry
  , postUrl
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeFileStrict, object, withObject, (.:), (.=))

-- | A post's title/date, as pulled from its markdown frontmatter by
-- pandoc via `frontmatterTemplate`. Kept separate from `PostEntry`:
-- these two fields are safe to round-trip through a literal
-- (non-escaping) pandoc template splice, since frontmatter values are
-- plain text -- unlike `PostEntry`'s `abstract`/`content`, which are
-- full of characters (quotes, newlines) that need real JSON escaping.
data PostFrontmatter = PostFrontmatter
  { fmTitle :: String
  , fmDate  :: String -- ^ "YYYY-MM-DD"
  , fmTags  :: String -- ^ verbatim frontmatter @tags@ value (e.g. "long,image"), or "" if unset
  }

instance FromJSON PostFrontmatter where
  parseJSON = withObject "post-frontmatter" $ \o ->
    PostFrontmatter <$> o .: "title" <*> o .: "date" <*> o .: "tags"

-- | pandoc template text that serializes a post's frontmatter
-- title/date/tags as the JSON the `FromJSON` instance above expects
-- back. A post with no @tags@ frontmatter field renders `$tags$` as an
-- empty string, same as an unset `$title$` would.
frontmatterTemplate :: String
frontmatterTemplate = "{\"title\": \"$title$\", \"date\": \"$date$\", \"tags\": \"$tags$\"}"

-- | Parse a rendered `frontmatterTemplate` file back into a
-- `PostFrontmatter`.
loadFrontmatter :: FilePath -> IO PostFrontmatter
loadFrontmatter jsonPath = either fail pure =<< eitherDecodeFileStrict jsonPath

data PostEntry = PostEntry
  { postTitle    :: String
  , postDate     :: String -- ^ "YYYY-MM-DD"
  , postSlug     :: String
  , postTags     :: String -- ^ verbatim frontmatter @tags@ value (e.g. "long,image"), or "" if unset
  , postAbstract :: Maybe String -- ^ rendered HTML, if the post has an @<!--more-->@ marker
  , postContent  :: String -- ^ the post's full rendered body HTML
  } deriving (Show)

instance ToJSON PostEntry where
  toJSON e =
    object
      [ "title" .= postTitle e
      , "date" .= postDate e
      , "slug" .= postSlug e
      , "tags" .= postTags e
      , "abstract" .= postAbstract e
      , "content" .= postContent e
      ]

instance FromJSON PostEntry where
  parseJSON = withObject "post-entry" $ \o ->
    PostEntry
      <$> o .: "title"
      <*> o .: "date"
      <*> o .: "slug"
      <*> o .: "tags"
      <*> o .: "abstract"
      <*> o .: "content"

-- | Parse a `PostEntry` previously written by `Data.Aeson.encodeFile`
-- (kiln's own cached entry json -- see `Kiln.Build.Post`).
loadPostEntry :: FilePath -> IO PostEntry
loadPostEntry jsonPath = either fail pure =<< eitherDecodeFileStrict jsonPath

-- | The site-relative URL a post is published at.
postUrl :: String -> PostEntry -> String
postUrl webroot e = webroot ++ "post/" ++ postSlug e ++ ".html"
