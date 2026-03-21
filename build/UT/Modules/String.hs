module UT.Modules.String (suiteName, testCases) where

import Modules.Utils.String
import UT.TestUtils.TestSuite
import UT.TestUtils.Asserts

-- Suite for core string replacement and trimming helpers.
suiteName :: String
suiteName = "String"

testCases :: [TestCase]
testCases =
  [ -- Test Case#1: Empty pattern replacement must be a no-op.
    mkTestCase "replace keeps input unchanged for empty old pattern" $
      assertEq "replace should keep input unchanged when old is empty" "abc" $
        replace "" "x" "abc"
  , -- Test Case#2: Replacement should handle repeated non-overlapping matches.
    mkTestCase "replace updates all non-overlapping matches" $ do
      assertEq "replace should replace all non-overlapping matches" "x b x" $
        replace "aa" "x" "aa b aa"
      assertEq "replace should leave unmatched content intact" "hello world" $
        replace "zzz" "x" "hello world"
  , -- Test Case#3: Trim should only remove leading and trailing whitespace.
    mkTestCase "trim removes edges and preserves inner spaces" $ do
      assertEq "trim should remove leading and trailing whitespace" "hello" $
        trim " \n\t hello \t "
      assertEq "trim should keep inner whitespace" "a  b" $
        trim "  a  b  "
  ]
