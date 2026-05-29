module Main where

import System.Environment (setEnv)
import System.Exit (exitFailure)
import Test.Framework.Colors
import Test.Framework.TestSuite (SuiteResult(..), runSuite)

import qualified Test.PT.Modules.Performance as UtPerformance

main :: IO ()
main = do
  setEnv "UT_ENABLE_PERF" "1"
  result <- runSuite UtPerformance.suiteName UtPerformance.testCases
  let successCount = suitePassed result
  let totalCount = suiteTotal result
  if successCount == totalCount
    then
      putStrLn $
        makeColor colorGreen ("All performance tests passed (" ++ show successCount ++ "/" ++ show totalCount ++ ").")
    else do
      putStrLn $
        makeColor colorRed ("Some performance tests failed (" ++ show successCount ++ "/" ++ show totalCount ++ " passed).")
      exitFailure
