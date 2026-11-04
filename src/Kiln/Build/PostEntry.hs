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

-- | A post's title, date and tags, as pandoc pulls them out of its
-- frontmatter. Separate from `PostEntry`, whose `postAbstract` and
-- `postContent` are rendered HTML rather than source values.
data PostFrontmatter = PostFrontmatter
  { fmTitle :: String
  , fmDate  :: String -- ^ "YYYY-MM-DD"
  , fmTags  :: String -- ^ comma-joined frontmatter @tags@ (e.g. "long,image"), or "" if unset
  } deriving (Show, Eq)

-- | The labels `frontmatterTemplate` prints under and
-- `parseFrontmatter` finds again.
titleLabel, dateLabel, tagsLabel :: String
titleLabel = "kiln-title:"
dateLabel = "kiln-date:"
tagsLabel = "kiln-tags:"

-- | pandoc template printing a post's title, date and tags, each on its
-- own labelled line.
--
-- Labelled lines rather than JSON, which needs escaping: a value is
-- whatever follows its label, so a straight quote or a multi-line block
-- scalar can't produce a document that doesn't parse.
--
-- @tags@ goes through @$for$@ so a bare @tags: long,image@ and a YAML
-- list @tags: [long, image]@ come out the same; @$tags$@ alone
-- concatenates a list with no separator.
frontmatterTemplate :: String
frontmatterTemplate =
  unlines
    [ titleLabel ++ " $title$"
    , dateLabel ++ " $date$"
    , tagsLabel ++ " $for(tags)$$tags$$sep$,$endfor$"
    ]

-- | Read a rendered `frontmatterTemplate` back. A missing label reads as
-- empty, exactly as an unset frontmatter key does.
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

-- | Read back a cached entry written by `Kiln.Build.Post`.
loadPostEntry :: FilePath -> IO PostEntry
loadPostEntry jsonPath = either fail pure =<< eitherDecodeFileStrict jsonPath
