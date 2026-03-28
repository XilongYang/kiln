module Modules.FontSubset where

import Data.Set (fromList, toList)
import Modules.Config
import System.Directory (listDirectory)
import System.FilePath
import System.Process (callProcess)

-- ---[ Overview ]------------------------------------------------------------
-- | Font subsetting pipeline helpers.
--
-- This module collects used characters from generated HTML files and invokes
-- @pyftsubset@ to produce the shipped subset font asset.

-- ---[ Public API ]------------------------------------------------------------

-- | Builds the unique character set used by font subsetting.
--
-- Input is index HTML plus all post HTML contents.
mkFontSet :: String -> [String] -> String
mkFontSet indexHtml postHtmls = (toList . fromList) (indexHtml ++ concat postHtmls)

-- ---[ Implementation Details ]-----------------------------------------------

-- | Builds @pyftsubset@ arguments for the production subset pipeline.
mkPyftsubsetArgs :: [String]
mkPyftsubsetArgs =
  [ originFontFilePath
  , "--text-file=" ++ fontSetPath
  , "--flavor=woff2"
  , "--output-file=" ++ subsetFontFilePath
  ]

-- | Generates subset font output from current site HTML files.
genFontSubset :: IO ()
genFontSubset = do
  indexHtml <- readFile indexPath 
  postNames <- listDirectory postPath
  let postPaths = map (\f -> postPath </> f) postNames 
  postHtmls <- mapM readFile postPaths

  let fontSet = mkFontSet indexHtml postHtmls

  writeFile fontSetPath fontSet 

  callProcess "pyftsubset" mkPyftsubsetArgs
