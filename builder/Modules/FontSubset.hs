module Modules.FontSubset where

import Data.Set (fromList, toList)
import Modules.Config
import System.Directory (listDirectory)
import System.FilePath
import System.Process (callProcess)

-- Builds the unique character set used by font subsetting from index/post html.
mkFontSet :: String -> [String] -> String
mkFontSet indexHtml postHtmls = (toList . fromList) (indexHtml ++ concat postHtmls)

-- Builds pyftsubset arguments used by the production font-subset pipeline.
mkPyftsubsetArgs :: [String]
mkPyftsubsetArgs =
  [ originFontFilePath
  , "--text-file=" ++ fontSetPath
  , "--flavor=woff2"
  , "--output-file=" ++ subsetFontFilePath
  ]

genFontSubset :: IO ()
genFontSubset = do
  indexHtml <- readFile indexPath 
  postNames <- listDirectory postPath
  let postPaths = map (\f -> postPath </> f) postNames 
  postHtmls <- mapM readFile postPaths

  let fontSet = mkFontSet indexHtml postHtmls

  writeFile fontSetPath fontSet 

  callProcess "pyftsubset" mkPyftsubsetArgs
