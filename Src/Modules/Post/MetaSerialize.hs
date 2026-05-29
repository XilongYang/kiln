module Modules.Post.MetaSerialize (serializeMeta) where

import Modules.Post
import Modules.Post.Internal

-- ---[ Overview ]------------------------------------------------------------
-- | Front-matter serializer for post metadata.
--
-- This module converts a 'PostMeta' value into the canonical markdown
-- front-matter block used by the build pipeline.

-- ---[ Public API ]------------------------------------------------------------

-- | Serializes metadata into a delimiter-wrapped front-matter block.
--
-- Output layout:
-- ---
-- title: @title@
-- author: @author@
-- date: @date@
-- ---
serializeMeta :: PostMeta -> String
serializeMeta meta = unlines 
  [ metaDelimiter 
  , "title: " ++ metaTitle meta
  , "author: " ++ metaAuthor meta
  , "date: " ++ metaDate meta
  , metaDelimiter
  ]

