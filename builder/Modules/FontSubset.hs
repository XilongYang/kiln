module Modules.FontSubset where

import Data.Set (fromList, toList)
import Modules.Config
import System.Directory (listDirectory)
import System.FilePath
import System.Process (callProcess)

genFontSubset :: IO ()
genFontSubset = do
  indexHtml <- readFile indexPath 
  postNames <- listDirectory postPath
  let postPaths = map (\f -> postPath </> f) postNames 
  postHtmls <- mapM readFile postPaths

  let fullContent = indexHtml ++(concat postHtmls)
  let fontSet = (toList . fromList) fullContent 

  writeFile fontSetPath fontSet 

  callProcess "pyftsubset"
    [ originFontFilePath 
    , "--text-file=" ++ fontSetPath 
    , "--flavor=woff2"
    , "--output-file=" ++ subsetFontFilePath 
    ]

