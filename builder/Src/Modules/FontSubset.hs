module Modules.FontSubset where

import qualified Data.Set as Set (Set, fromList, toList, union, empty) 
import Modules.Config
import System.Directory (listDirectory)
import System.FilePath
import System.Process (callProcess)
import Control.Monad (foldM)

-- ---[ Overview ]------------------------------------------------------------
-- | Font subsetting pipeline helpers.
--
-- This module collects used characters from generated HTML files and invokes
-- @pyftsubset@ to produce the shipped subset font asset.

-- ---[ Public API ]------------------------------------------------------------

-- | Builds the unique character set used by font subsetting.
--
-- Input is index HTML plus all post HTML contents.
mkFontSet :: String -> String
mkFontSet html = (Set.toList . Set.fromList) html

-- | Builds @pyftsubset@ arguments for the production subset pipeline.
mkPyftsubsetArgs :: [String]
mkPyftsubsetArgs =
  [ originFontFilePath
  , "--text-file=" ++ fontSetPath
  , "--flavor=woff2"
  , "--output-file=" ++ subsetFontFilePath
  ]

type CharSet = Set.Set Char

-- | Generates subset font output from current site HTML files.
genFontSubset :: IO ()
genFontSubset = do
  charsetNames <- listDirectory charsetArtifactsPath
  let charsetPaths = map (\f -> charsetArtifactsPath </> f) $ filter (\f -> takeExtension f == ".txt") charsetNames
  charset <- foldM updateCharset (Set.empty) charsetPaths 

  writeFile fontSetPath (Set.toList charset)

  callProcess "pyftsubset" mkPyftsubsetArgs

updateCharset :: CharSet -> FilePath -> IO CharSet
updateCharset currentSet path = do
  str <- readFile path
  pure $ Set.union currentSet (Set.fromList str)
