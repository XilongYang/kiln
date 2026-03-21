module UT.TestUtils.TestSuite
  ( TestCase
  , mkTestCase
  , runSuite
  ) where

import Control.Exception (SomeException, displayException, try)

-- Minimal test helpers for the local UT runner.
--
-- This module intentionally stays dependency-free and only relies on `base`,
-- so tests can be executed directly with `runghc` without introducing a
-- separate testing framework.
type TestCase = (String, IO ())

-- Constructs a named test case for use in a suite.
--
-- The title is used in console output, and the action is expected to throw on
-- failure through one of the assertion helpers below.
mkTestCase :: String -> IO () -> TestCase
mkTestCase title action = (title, action)

-- Runs a named suite and reports each case independently.
--
-- Failures are caught per test case so the remaining cases in the same suite
-- still execute. The returned `Bool` indicates whether the full suite passed.
runSuite :: String -> [TestCase] -> IO Bool
runSuite suiteName cases = do
  putStrLn ("Test Suite " ++ suiteName)
  passed <- mapM runIndexedCase (zip [1 :: Int ..] cases)
  let okCount = length (filter id passed)
  let totalCount = length cases
  putStrLn ""
  putStrLn ("Summary: " ++ show okCount ++ "/" ++ show totalCount ++ " Tests Passed")
  putStrLn ""
  pure (okCount == totalCount)
  where
    runIndexedCase :: (Int, TestCase) -> IO Bool
    runIndexedCase (idx, (title, action)) = do
      result <- try action :: IO (Either SomeException ())
      case result of
        Right _ -> do
          putStrLn ("[OK] Test Case#" ++ show idx ++ " " ++ title)
          pure True
        Left err -> do
          putStrLn ("[NG] Test Case#" ++ show idx ++ " " ++ title)
          putStrLn ("Message: " ++ displayException err)
          pure False

