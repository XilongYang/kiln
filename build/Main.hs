module Main where

import Modules.Config
import Modules.Template
import Modules.Utils.TempDir

import System.IO (writeFile, readFile)
import System.Process (callProcess)

main :: IO ()
main = withTempDir $ do
  templatePost <- genTemplate templatePostPath 
  writeFile tempTemplatePostPath templatePost
  templateIndex <- genTemplate templateIndexPath 
  writeFile tempTemplateIndexPath templateIndex

  callProcess "pandoc"
    [ srcPath ++ "/" ++ "Dabble_in_Machine_Learning_1_Prelude_and_Categories.md"
    , "--template=" ++ tempTemplatePostPath
    , "-o", postPath ++ "/" ++ "hello.html"
    ]


