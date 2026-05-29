module Test.Framework.TestSuite
  ( TestCase
  , SuiteResult(..)
  , mkTestCase
  , runSuite
  ) where

import Control.Exception (SomeException, displayException, try)
import Test.Framework.Colors

-- Pre-colored status tags for per-test output lines.
okTag :: String
okTag = makeColor colorGreen "[OK]"

ngTag :: String
ngTag = makeColor colorRed "[NG]" 

-- Minimal test helpers for the local UT runner.
--
-- This module intentionally stays dependency-free and only relies on `base`,
-- so tests can be executed directly with `runghc` without introducing a
-- separate testing framework.
type TestCase = (String, IO ())

data SuiteResult = SuiteResult
  { suitePassed :: Int
  , suiteTotal :: Int
  }

-- Constructs a named test case for use in a suite.
--
-- The title is used in console output, and the action is expected to throw on
-- failure through one of the assertion helpers below.
mkTestCase :: String -> IO () -> TestCase
mkTestCase title action = (title, action)

-- Runs a named suite and reports each case independently.
--
-- Failures are caught per test case so the remaining cases in the same suite
-- still execute. The returned result contains passed/total counts for this
-- suite.
runSuite :: String -> [TestCase] -> IO SuiteResult
runSuite suiteName cases = do
  putStrLn ("Test Suite " ++ suiteName)
  passed <- mapM runIndexedCase (zip [1 :: Int ..] cases)
  let okCount = length (filter id passed)
  let totalCount = length cases
  putStrLn ""
  putStrLn ("Summary: " ++ show okCount ++ "/" ++ show totalCount ++ " Tests Passed")
  putStrLn ""
  pure (SuiteResult okCount totalCount)
  where
    runIndexedCase :: (Int, TestCase) -> IO Bool
    runIndexedCase (idx, (title, action)) = do
      result <- try action :: IO (Either SomeException ())
      case result of
        -- Pass path: print the green marker and continue.
        Right _ -> do
          putStrLn (okTag ++ " Test Case#" ++ show idx ++ " " ++ title)
          pure True
        -- Fail path: print the red marker and surface exception message.
        Left err -> do
          putStrLn (ngTag ++ " Test Case#" ++ show idx ++ " " ++ title)
          putStrLn ("Message: " ++ displayException err)
          pure False
