module Kiln.Build.Template
  ( renderComponents
  , replaceAll
  , loadComponents
  , substituteComponentsInFile
  ) where

import Data.List (isPrefixOf)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import System.IO (readFile')

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

-- | Read every file in `dir` into a (basename, content) pair, for use as
-- the component list `renderComponents` expects.
loadComponents :: FilePath -> IO [(String, String)]
loadComponents dir = do
  names <- listDirectory dir
  mapM (\name -> (,) (takeBaseName name) <$> readFile' (dir </> name)) names

-- | Apply `renderComponents` to the file at `dir </> name`, in place.
substituteComponentsInFile :: FilePath -> [(String, String)] -> FilePath -> IO ()
substituteComponentsInFile dir components name = do
  let path = dir </> name
  content <- readFile' path
  writeFile path (renderComponents components content)
