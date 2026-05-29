module Modules.Post.Internal (metaDelimiter) where

-- ---[ Overview ]------------------------------------------------------------
-- | Internal constants shared by post parsing helpers.
--
-- This module groups implementation-detail values that are intentionally kept
-- out of the main post domain module.

-- ---[ Public API ]------------------------------------------------------------

-- | Front-matter block delimiter used in markdown post files.
--
-- Example:
-- ---
-- title: Example
-- ---
metaDelimiter :: String
metaDelimiter  = "---"

