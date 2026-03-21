module Modules.Utils.String where

import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- Replaces all non-overlapping occurrences of a substring (old) with another substring (new) in the input string.
-- If 'old' is the empty string, the input is returned unchanged.
replace :: String -> String -> String -> String
replace [] _ = id
replace old new = replace'
  where
    replace' [] = []
    replace' s@(x:xs) 
     | old `isPrefixOf` s = new ++ replace' (drop (length old) s)
     | otherwise = x:(replace' xs)


-- Removes leading and trailing whitespace from the input string.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

