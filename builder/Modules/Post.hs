module Modules.Post (Post (..), PostMeta (..)) where

import Modules.TypeAlias
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Domain types for parsed blog posts.
--
-- This module defines the in-memory structures used across build stages after
-- source markdown files are parsed.

-- ---[ Public API ]------------------------------------------------------------

-- | Canonical parsed post payload used by builders.
data Post = Post
  { postName       :: String    -- ^ Basename (without extension) of source file.
  , postSourcePath :: FilePath  -- ^ Repository-relative source path.
  , postBody       :: Markdown  -- ^ Full markdown body content.
  , postAbstract   :: Markdown  -- ^ Extracted abstract fragment.
  , postMeta       :: PostMeta  -- ^ Parsed front-matter metadata.
  } deriving (Show, Eq)

-- | Front-matter metadata extracted from a source post.
data PostMeta = PostMeta
  { metaTitle  :: String   -- ^ Post title.
  , metaAuthor :: String   -- ^ Post author name.
  , metaDate   :: String   -- ^ Post date string from front matter.
  } deriving (Show, Eq)

