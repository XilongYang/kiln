module Modules.Toc (injectToc) where

import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Modules.TypeAlias (Html)
import Modules.Utils.String

-- ---[ Overview ]------------------------------------------------------------
-- | Table-of-contents generator and injector for rendered post HTML.
--
-- This module scans heading lines after the first @<main...>@ tag, builds a
-- nested TOC list, and replaces the @[[toc]]@ marker in the page.

-- ---[ Public API ]------------------------------------------------------------

-- | Replaces @[[toc]]@ with a generated navigation block.
--
-- The generated TOC is derived from heading lines found under @<main>@.
injectToc :: Html -> Html
injectToc html = replace "[[toc]]" toc html
  where 
    toc = unlines
      [ "<nav role=\"navigation\" class=\"toc\">"
      , "<h2>Contents"
      , "<i class=\"material-icons icon\" id=\"toc-control\">remove</i>"
      , "</h2>"
      , "<div id=\"toc-items\">"
      , tocs html
      , "</div>"
      , "</nav>"
      ]
    (_, main) = ((break ("<main" `isPrefixOf`)) . (map trim) . lines) html
    headItems = catMaybes $ map extractHeadItem main
    tocs html = "<ol>" ++ tocItems headItems ++ "</ol>"

-- ---[ Implementation Details ]-----------------------------------------------

-- | Parses one heading line into @(level, id, title)@.
--
-- Returns 'Nothing' for non-heading or malformed lines.
extractHeadItem :: Html -> Maybe (String, String, String)
extractHeadItem line
  | not ("<h" `isPrefixOf` line) = Nothing
  | otherwise = case break (== '>') line of
    (openTag, '>':rest) ->
      let level = [openTag !! 2]
          ident = extractIdent openTag 
          title = takeWhile (/= '<') rest
      in Just (level, ident, title)
    otherwise -> Nothing
  where
    extractIdent [] = ""
    extractIdent (x:xs) 
      | "id=\"" `isPrefixOf` xs = takeWhile (/= '"') (drop 4 xs)
      | otherwise = extractIdent xs

-- | Renders nested TOC HTML from ordered heading tuples.
--
-- The nesting rule groups consecutive deeper headings under the current item.
tocItems :: [(String, String, String)] -> Html
tocItems [] = ""
tocItems ((level, ident, title) : xs) = unlines
  [ "<li><a href=\"#" ++ ident ++"\">" ++ title ++ "</a><ol>"
  , tocItems subItems
  , "</ol></li>"
  , tocItems restItems
  ]
  where
    (subItems, restItems) = break (\(curLevel, _, _) -> level >= curLevel) xs
