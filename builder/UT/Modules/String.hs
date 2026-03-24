module UT.Modules.String (suiteName, testCases) where

import Modules.Utils.String
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for core string replacement and trimming helpers.
suiteName :: String
suiteName = "String"

testCases :: [TestCase]
testCases =
  [ mkTestCase "replace treats an empty search pattern as a no-op" $
      assertEq "replace should keep input unchanged when old is empty" "abc" $
        replace "" "x" "abc"
  , mkTestCase "replace remains a no-op when both patterns are empty" $
      assertEq "replace should keep input unchanged when old is empty" "abc" $
        replace "" "" "abc"
  , mkTestCase "replace preserves identity on empty input under no-op rules" $
      assertEq "replace should keep input unchanged when old is empty" "" $
        replace "" "" ""
  , mkTestCase "replace applies substitution consistently across the whole input" $
      assertEq "replace should replace all non-overlapping matches" "x b x" $
        replace "aa" "x" "aa b aa"
  , mkTestCase "replace supports deletion semantics via empty replacement" $
      assertEq "replace should replace all non-overlapping matches" " b " $
        replace "aa" "" "aa b aa"
  , mkTestCase "replace is non-destructive when no match exists" $
      assertEq "replace should leave unmatched content intact" "hello world" $
        replace "zzz" "x" "hello world"
  , mkTestCase "replace keeps empty input stable when there is nothing to match" $
      assertEq "replace should leave unmatched content intact" "" $
        replace "zzz" "x" ""
  , mkTestCase "trim normalizes whitespace-only input to canonical empty output" $
      assertEq "trim should remove leading and trailing whitespace" "" $
        trim " \n\t  \t "
  , mkTestCase "trim removes boundary noise while preserving the core token" $
      assertEq "trim should remove leading and trailing whitespace" "hello" $
        trim " \n\t hello \t "
  , mkTestCase "trim preserves meaningful internal spacing" $
      assertEq "trim should keep inner whitespace" "a  b" $
        trim "  a  b  "
  ]
