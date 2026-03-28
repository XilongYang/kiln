module Modules.Utils.String where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- ---[ Overview ]------------------------------------------------------------
-- | Small string helpers used across builder modules.
--
-- This module keeps simple, dependency-free string utilities for text
-- rewriting and whitespace normalization.

-- ---[ Public API ]------------------------------------------------------------

-- | Replaces all non-overlapping occurrences of @old@ with @new@.
--
-- If @old@ is empty, returns the input unchanged.
replace :: String -> String -> String -> String
replace [] _ = id
replace old new = replace'
  where
    replace' [] = []
    replace' s@(x:xs) 
     | old `isPrefixOf` s = new ++ replace' (drop (length old) s)
     | otherwise = x:(replace' xs)

-- | Removes leading and trailing whitespace.
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

