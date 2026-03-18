module Modules.Utils.String where

import Data.List (isPrefixOf)

-- replace old -> new -> text -> result
replace :: String -> String -> String -> String
replace [] _ = id
replace old new = replace'
  where
    replace' [] = []
    replace' s@(x:xs) 
     | old `isPrefixOf` s = new ++ replace' (drop (length old) s)
     | otherwise = x:(replace' xs)

