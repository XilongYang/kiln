module Main where

import UT.TestUtils.TestSuite (runSuite)

import qualified UT.Modules.String as UtString
import qualified UT.Modules.Template as UtTemplate
import qualified UT.Modules.TempDir as UtTempDir

main :: IO ()
main = do
  results <-
    sequence
      [ runSuite UtString.suiteName UtString.testCases
      , runSuite UtTemplate.suiteName UtTemplate.testCases
      , runSuite UtTempDir.suiteName UtTempDir.testCases
      ]
  if and results
    then putStrLn "All UT passed."
    else error "Some UT failed."
