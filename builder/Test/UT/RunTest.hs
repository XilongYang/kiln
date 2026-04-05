module Main where

import System.Exit (exitFailure)
import Test.Framework.Colors
import Test.Framework.TestSuite (SuiteResult(..), runSuite)

import qualified Test.UT.Modules.Builder as UtBuilder
import qualified Test.UT.Modules.BuildJudger as UtBuildJudger
import qualified Test.UT.Modules.BuildPlan as UtBuildPlan
import qualified Test.UT.Modules.Config as UtConfig
import qualified Test.UT.Modules.FontSubset as UtFontSubset
import qualified Test.UT.Modules.Index.Render as UtIndexRender
import qualified Test.UT.Modules.Index.Item as UtIndexItem
import qualified Test.UT.Modules.Main as UtMain
import qualified Test.UT.Modules.Post.Parse as UtPostParse
import qualified Test.UT.Modules.Post.Preprocess as UtPostPreprocess
import qualified Test.UT.Modules.SearchDB as UtSearchDB
import qualified Test.UT.Modules.Template as UtTemplate
import qualified Test.UT.Modules.Toc as UtToc
import qualified Test.UT.Modules.TypeAlias as UtTypeAlias
import qualified Test.UT.Modules.Utils.OrphanCheck as UtOrphanCheck
import qualified Test.UT.Modules.Utils.Klb as UtKlb
import qualified Test.UT.Modules.Utils.Sha256 as UtSha256
import qualified Test.UT.Modules.Utils.String as UtString
import qualified Test.UT.Modules.Utils.TempDir as UtTempDir

main :: IO ()
main = do
  results <-
    sequence
      [ runSuite UtString.suiteName UtString.testCases
      , runSuite UtSha256.suiteName UtSha256.testCases
      , runSuite UtKlb.suiteName UtKlb.testCases
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
