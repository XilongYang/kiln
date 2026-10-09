module Kiln.Str
  ( trim
  , replaceAll
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

replaceAll :: String -> String -> String -> String
replaceAll needle replacement = go
  where
    go [] = []
    go s@(c : cs)
      | needle `isPrefixOf` s = replacement ++ go (drop (length needle) s)
      | otherwise = c : go cs
