module Modules.Builder.IndexBuilder where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Modules.IndexItem
import Modules.TypeAlias
import Modules.Utils.String

-- Fills an index template by injecting generated post-list HTML.
genIndexHtml :: [IndexItem] -> Html -> Html
genIndexHtml items template = 
  replace "$posts$" itemsHtml template 
  where
    itemsHtml = unlines 
      [ "<div class=\"post-wrapper\">"
      , yearGroupdHtml items
      , "</div>"
      ]

-- Groups items by year and renders each year section.
yearGroupdHtml :: [IndexItem] -> Html
yearGroupdHtml = unlines . (map singleYearHtml). groupItemsByYear 

-- Renders one year group heading and its post rows.
singleYearHtml :: (String, [IndexItem]) -> Html
singleYearHtml (year, items) = unlines 
  [ "<div class=\"post-year-wrapper\">"
  , "<h3>" ++ year ++ "</h3>"
  ++ itemsToHtml items
  , "</div>"
  ]

-- Renders year-local post rows in reverse chronological order.
--
-- Ordering key is `(month, day)` descending, with month/day represented as
-- zero-padded strings from source metadata.
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

-- Groups items by year and returns years in descending order.
groupItemsByYear :: [IndexItem] -> [(String, [IndexItem])]
groupItemsByYear = reverse . mergeItemsWithYear . (map itemWithYear)
  where
    itemWithYear item = (itemYear item, item) 

-- Collects `(year, item)` pairs into `(year, [item])`.
mergeItemsWithYear :: [(String, IndexItem)] -> [(String, [IndexItem])]
mergeItemsWithYear pairs = Map.toList $
  Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]
