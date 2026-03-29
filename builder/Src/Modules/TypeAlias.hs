module Modules.TypeAlias where

-- ---[ Overview ]------------------------------------------------------------
-- | Shared semantic type aliases used across modules.
--
-- These aliases keep signatures self-descriptive without introducing runtime
-- overhead or new data constructors.

-- ---[ Public API ]------------------------------------------------------------

-- | Raw markdown text of a post file.
type Markdown = String
-- | Public URL used in generated pages.
type Url = String
-- | Rendered HTML fragment or full document content.
type Html = String
-- | JSON text used in generated @searchdb@ file.
type SearchJson = String

