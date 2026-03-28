module Modules.Post.Preprocess (preprocessPost) where

import Data.List (isPrefixOf)
import Modules.Post
import Modules.Post.MetaSerialize
import Modules.Utils.String
import Modules.TypeAlias (Markdown)

-- ---[ Overview ]------------------------------------------------------------
-- | Markdown preprocessing for post build input.
--
-- This module transforms parsed post data into the intermediate markdown fed
-- to downstream HTML generation.

-- ---[ Public API ]------------------------------------------------------------

-- | Builds preprocessed markdown text from a parsed 'Post'.
--
-- Output layout:
-- - serialized front matter
-- - abstract wrapper block
-- - toc marker (@[[toc]]@)
-- - body markdown with rewritten language fence openers
preprocessPost :: Post -> Markdown
preprocessPost post = unlines
  [ rawMeta
  , abstract
  , "[[toc]]\n"
  , rewriteLanguageMarks $ postBody post
  ]
  where
    rawMeta = serializeMeta $ postMeta post
    abstract = unlines
      [ "<div class=\"abstract\">"
      , postAbstract post
      , "</div>"
      ]

-- ---[ Implementation Details ]-----------------------------------------------

-- | Rewrites fenced code block opener lines with language class attributes.
--
-- Examples:
-- - @```haskell@ -> @``` {.language-haskell .line-numbers .match-braces}@
-- - @```@ remains unchanged
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks = 
  unlines . map rewriteLanguageMarkLine . lines
  where
    rewriteLanguageMarkLine :: String -> String
    rewriteLanguageMarkLine line
      | not $  "```" `isPrefixOf` line = line
      | mark == "" = line
      | otherwise = replace "[mark]" mark "``` {.language-[mark] .line-numbers .match-braces}"
      where
        mark = trim $ drop 3 line
