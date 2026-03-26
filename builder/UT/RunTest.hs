module Main where

import System.Exit (exitFailure)
import UT.TestUtils.Colors
import UT.TestUtils.TestSuite (runSuite)

import qualified UT.Modules.Builder as UtBuilder
import qualified UT.Modules.BuildPlan as UtBuildPlan
import qualified UT.Modules.Config as UtConfig
import qualified UT.Modules.FontSubset as UtFontSubset
import qualified UT.Modules.IndexBuilder as UtIndexBuilder
import qualified UT.Modules.IndexItem as UtIndexItem
import qualified UT.Modules.Main as UtMain
import qualified UT.Modules.OrphanCheck as UtOrphanCheck
import qualified UT.Modules.Post as UtPost
import qualified UT.Modules.PostBuilder as UtPostBuilder
import qualified UT.Modules.SearchDB as UtSearchDB
import qualified UT.Modules.Sha256 as UtSha256
import qualified UT.Modules.String as UtString
import qualified UT.Modules.Template as UtTemplate
import qualified UT.Modules.TempDir as UtTempDir
import qualified UT.Modules.TypeAlias as UtTypeAlias

main :: IO ()
main = do
  results <-
    sequence
      [ runSuite UtString.suiteName UtString.testCases
      , runSuite UtSha256.suiteName UtSha256.testCases
      , runSuite UtTypeAlias.suiteName UtTypeAlias.testCases
      , runSuite UtConfig.suiteName UtConfig.testCases
      , runSuite UtTemplate.suiteName UtTemplate.testCases
      , runSuite UtTempDir.suiteName UtTempDir.testCases
      , runSuite UtPost.suiteName UtPost.testCases
      , runSuite UtIndexItem.suiteName UtIndexItem.testCases
      , runSuite UtOrphanCheck.suiteName UtOrphanCheck.testCases
      , runSuite UtBuildPlan.suiteName UtBuildPlan.testCases
      , runSuite UtPostBuilder.suiteName UtPostBuilder.testCases
      , runSuite UtIndexBuilder.suiteName UtIndexBuilder.testCases
      , runSuite UtSearchDB.suiteName UtSearchDB.testCases
      , runSuite UtFontSubset.suiteName UtFontSubset.testCases
      , runSuite UtBuilder.suiteName UtBuilder.testCases
      , runSuite UtMain.suiteName UtMain.testCases
      ]
  if and results
    then putStrLn $ makeColor colorGreen "All UT passed."
    else do
      putStrLn $ makeColor colorRed "Some UT failed."
      exitFailure
