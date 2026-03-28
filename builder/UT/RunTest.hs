module Main where

import System.Exit (exitFailure)
import UT.TestUtils.Colors
import UT.TestUtils.TestSuite (SuiteResult(..), runSuite)

import qualified UT.Modules.Builder as UtBuilder
import qualified UT.Modules.BuildJudger as UtBuildJudger
import qualified UT.Modules.BuildPlan as UtBuildPlan
import qualified UT.Modules.Config as UtConfig
import qualified UT.Modules.FontSubset as UtFontSubset
import qualified UT.Modules.Index.Render as UtIndexRender
import qualified UT.Modules.Index.Item as UtIndexItem
import qualified UT.Modules.Main as UtMain
import qualified UT.Modules.Performance as UtPerformance
import qualified UT.Modules.Post.Parse as UtPostParse
import qualified UT.Modules.Post.Preprocess as UtPostPreprocess
import qualified UT.Modules.SearchDB as UtSearchDB
import qualified UT.Modules.Template as UtTemplate
import qualified UT.Modules.Toc as UtToc
import qualified UT.Modules.TypeAlias as UtTypeAlias
import qualified UT.Modules.Utils.OrphanCheck as UtOrphanCheck
import qualified UT.Modules.Utils.Sha256 as UtSha256
import qualified UT.Modules.Utils.String as UtString
import qualified UT.Modules.Utils.TempDir as UtTempDir

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
      , runSuite UtPostParse.suiteName UtPostParse.testCases
      , runSuite UtIndexItem.suiteName UtIndexItem.testCases
      , runSuite UtOrphanCheck.suiteName UtOrphanCheck.testCases
      , runSuite UtBuildPlan.suiteName UtBuildPlan.testCases
      , runSuite UtBuildJudger.suiteName UtBuildJudger.testCases
      , runSuite UtPostPreprocess.suiteName UtPostPreprocess.testCases
      , runSuite UtToc.suiteName UtToc.testCases
      , runSuite UtIndexRender.suiteName UtIndexRender.testCases
      , runSuite UtSearchDB.suiteName UtSearchDB.testCases
      , runSuite UtFontSubset.suiteName UtFontSubset.testCases
      , runSuite UtBuilder.suiteName UtBuilder.testCases
      , runSuite UtMain.suiteName UtMain.testCases
      , runSuite UtPerformance.suiteName UtPerformance.testCases
      ]
  let successCount = sum (map suitePassed results)
  let totalCount = sum (map suiteTotal results)
  if successCount == totalCount
    then
      putStrLn $
        makeColor colorGreen ("All UT passed (" ++ show successCount ++ "/" ++ show totalCount ++ ").")
    else do
      putStrLn $
        makeColor colorRed ("Some UT failed (" ++ show successCount ++ "/" ++ show totalCount ++ " passed).")
      exitFailure
