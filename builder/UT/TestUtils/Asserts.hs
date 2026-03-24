module UT.TestUtils.Asserts
  ( assertTrue
  , assertFalse
  , assertEq
  , assertNotEq
  , assertContains
  , assertThrows
  ) where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isInfixOf)

-- Aborts the current test case with a formatted assertion failure message.
failTest :: String -> IO ()
failTest message = error ("Assertion failed: " ++ message)

-- Asserts that a condition is `True`.
assertTrue :: String -> Bool -> IO ()
assertTrue message cond =
  if cond
    then pure ()
    else failTest message

-- Asserts that a condition is `False`.
assertFalse :: String -> Bool -> IO ()
assertFalse message cond = assertTrue message (not cond)

-- Asserts structural equality and reports both expected and actual values.
assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq message expected actual =
  if expected == actual
    then pure ()
    else
      failTest
        (message ++ "\nexpected: " ++ show expected ++ "\nactual:   " ++ show actual)

-- Asserts that two values are observably different.
assertNotEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertNotEq message left right =
  if left /= right
    then pure ()
    else failTest (message ++ "\nboth values: " ++ show left)

-- Asserts that the given substring occurs within the target string.
assertContains :: String -> String -> String -> IO ()
assertContains message needle haystack =
  if needle `isInfixOf` haystack
    then pure ()
    else
      failTest
        (message ++ "\nmissing: " ++ show needle ++ "\nin:      " ++ show haystack)

-- Asserts that evaluating the supplied action raises an exception.
--
-- The action result is forced to WHNF so failures hidden in lazy evaluation are
-- also observed by the test runner.
assertThrows :: String -> IO a -> IO ()
assertThrows message action = do
  result <- try (action >>= evaluate >> pure ()) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> failTest (message ++ "\nexpected exception, but action succeeded")
