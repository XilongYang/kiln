module Main where

import UT.TestUtils.TestSuite (runSuite)
import UT.TestUtils.Colors

import qualified UT.Modules.BuildPlan as UtBuildPlan
import qualified UT.Modules.OrphanCheck as UtOrphanCheck
import qualified UT.Modules.Post as UtPost
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
      , runSuite UtPost.suiteName UtPost.testCases
      , runSuite UtOrphanCheck.suiteName UtOrphanCheck.testCases
      , runSuite UtBuildPlan.suiteName UtBuildPlan.testCases
      ]
  if and results
    then putStrLn $ makeColor colorGreen "All UT passed."
    else putStrLn $ makeColor colorRed "Some UT failed."
