module Modules.Index.Render (renderIndex) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Modules.Index.Item
import Modules.TypeAlias
import Modules.Utils.String

-- ---[ Overview ]------------------------------------------------------------
-- | Index-page renderer for grouped post listings.
--
-- This module takes prepared 'IndexItem' values, groups them by year, renders
-- list HTML, and injects it into the index template.

-- ---[ Public API ]------------------------------------------------------------

-- | Injects grouped post-list HTML into the index template.
--
-- The placeholder token replaced is @$posts$@.
renderIndex :: [IndexItem] -> Html -> Html
renderIndex items template = 
  replace "$posts$" itemsHtml template 
  where
    itemsHtml = unlines 
      [ "<div class=\"post-wrapper\">"
      , yearGroupedHtml items
      , "</div>"
      ]

-- ---[ Implementation Details ]-----------------------------------------------

-- | Renders full grouped list HTML by year.
yearGroupedHtml :: [IndexItem] -> Html
yearGroupedHtml = unlines . (map singleYearHtml). groupItemsByYear 

-- | Renders one year group block.
singleYearHtml :: (String, [IndexItem]) -> Html
singleYearHtml (year, items) = unlines 
  [ "<div class=\"post-year-wrapper\">"
  , "<h3>" ++ year ++ "</h3>"
  ++ itemsToHtml items
  , "</div>"
  ]

-- | Renders item rows ordered by month/day descending within one year.
itemsToHtml :: [IndexItem] -> Html
itemsToHtml = unlines . (map itemToHtml) . 
  reverse .
  (sortOn (\x -> (itemMonth x, itemDate x)))
  where
    itemToHtml item = unlines 
      [ "<div class=\"post-wrapper\">"
      , "<p>" ++ itemDate item 
      ++" <a href=\"" ++ itemUrl item 
      ++ "\">" ++ itemTitle item 
      ++ "</a></p>"
      , "</div>"
      ]
    itemDate item = itemMonth item ++ "-" ++ itemDay item

-- | Groups index items by year, with newer years first.
groupItemsByYear :: [IndexItem] -> [(String, [IndexItem])]
groupItemsByYear = reverse . mergeItemsWithYear . (map itemWithYear)
  where
    itemWithYear item = (itemYear item, item) 

-- | Merges year-tagged items using a strict map accumulator.
mergeItemsWithYear :: [(String, IndexItem)] -> [(String, [IndexItem])]
mergeItemsWithYear pairs = Map.toList $
  Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]
