{-# LANGUAGE OverloadedStrings #-}

module Kiln.Build.PostEntry
  ( PostEntry (..)
  , PostFrontmatter (..)
  , frontmatterTemplate
  , parseFrontmatter
  , loadFrontmatter
  , loadPostEntry
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeFileStrict, object, withObject, (.:), (.=))
import Data.List (find, isPrefixOf)
import Kiln.Str (trim)
import System.IO (readFile')

-- | A post's title, date and tags, as pulled from its markdown
-- frontmatter by pandoc via `frontmatterTemplate`. Kept separate from
-- `PostEntry`: these come straight back out of the source document,
-- whereas `PostEntry`'s `postAbstract`/`postContent` are rendered HTML.
data PostFrontmatter = PostFrontmatter
  { fmTitle :: String
  , fmDate  :: String -- ^ "YYYY-MM-DD"
  , fmTags  :: String -- ^ comma-joined frontmatter @tags@ (e.g. "long,image"), or "" if unset
  } deriving (Show, Eq)

-- | The label each frontmatter field is printed under by
-- `frontmatterTemplate` and found again by `parseFrontmatter`.
titleLabel, dateLabel, tagsLabel :: String
titleLabel = "kiln-title:"
dateLabel = "kiln-date:"
tagsLabel = "kiln-tags:"

-- | pandoc template text that prints a post's frontmatter title, date
-- and tags, each on its own labelled line.
--
-- Deliberately not JSON. This used to be a hand-built JSON object with
-- the values interpolated into it, which meant any value carrying a
-- character JSON escapes produced a document that didn't parse -- a
-- straight quote surviving pandoc's smart quotes (one inside a code
-- span, say), or the newlines of a multi-line YAML block scalar -- and
-- the build died on an aeson error naming neither the field nor the
-- post. Labelled lines need no escaping whatsoever: a value is whatever
-- follows its label, and one that runs onto further lines simply keeps
-- what precedes the next label.
--
-- @tags@ is printed through @$for$@ so that both spellings a post might
-- use -- a bare string @tags: long,image@ and a YAML list
-- @tags: [long, image]@ -- come out as the same comma-separated value;
-- interpolating @$tags$@ directly concatenated a list's items with no
-- separator at all.
frontmatterTemplate :: String
frontmatterTemplate =
  unlines
    [ titleLabel ++ " $title$"
    , dateLabel ++ " $date$"
    , tagsLabel ++ " $for(tags)$$tags$$sep$,$endfor$"
    ]

-- | Read a rendered `frontmatterTemplate` back. A field whose label is
-- absent reads as empty, exactly as an unset frontmatter key does.
parseFrontmatter :: String -> PostFrontmatter
parseFrontmatter rendered =
  PostFrontmatter
    { fmTitle = field titleLabel
    , fmDate = field dateLabel
    , fmTags = field tagsLabel
    }
  where
    ls = lines rendered
    field label = maybe "" (trim . drop (length label)) (find (label `isPrefixOf`) ls)

loadFrontmatter :: FilePath -> IO PostFrontmatter
loadFrontmatter path = parseFrontmatter <$> readFile' path

data PostEntry = PostEntry
  { postTitle    :: String
  , postDate     :: String -- ^ "YYYY-MM-DD"
  , postSlug     :: String
  , postTags     :: String -- ^ comma-joined frontmatter @tags@ (e.g. "long,image"), or "" if unset
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
