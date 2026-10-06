module Kiln.Template (renderComponents, replaceAll) where

import Data.List (isPrefixOf)

-- | Replace every @\<!--\<name\>--\>@ placeholder in the page with the
-- matching entry from the component list.
renderComponents :: [(String, String)] -> String -> String
renderComponents components page = foldl applyOne page components
  where
    applyOne acc (name, content) = replaceAll (placeholder name) content acc

placeholder :: String -> String
placeholder name = "<!--<" ++ name ++ ">-->"

replaceAll :: String -> String -> String -> String
replaceAll needle replacement = go
  where
    go [] = []
    go s@(c : cs)
      | needle `isPrefixOf` s = replacement ++ go (drop (length needle) s)
      | otherwise = c : go cs
