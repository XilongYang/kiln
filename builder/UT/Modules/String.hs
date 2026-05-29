module UT.Modules.String (suiteName, testCases) where

import Modules.Utils.String
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for core string replacement and trimming helpers.
suiteName :: String
suiteName = "String"

testCases :: [TestCase]
testCases =
  [ testReplaceEmptyOldNoOp
  , testReplaceBothEmptyNoOp
  , testReplaceEmptyInputIdentity
  , testReplaceAllMatches
  , testReplaceDeletion
  , testReplaceNoMatch
  , testReplaceNoMatchOnEmptyInput
  , testTrimWhitespaceOnly
  , testTrimBoundaryWhitespace
  , testTrimKeepsInnerWhitespace
  ]

-- Confirms replace is a no-op when the search pattern is empty.
testReplaceEmptyOldNoOp :: TestCase
testReplaceEmptyOldNoOp =
  mkTestCase "replace treats an empty search pattern as a no-op" $
    assertEq "replace should keep input unchanged when old is empty" "abc" $
      replace "" "x" "abc"

-- Confirms replace stays a no-op when both old/new patterns are empty.
testReplaceBothEmptyNoOp :: TestCase
testReplaceBothEmptyNoOp =
  mkTestCase "replace remains a no-op when both patterns are empty" $
    assertEq "replace should keep input unchanged when old is empty" "abc" $
      replace "" "" "abc"

-- Confirms replace preserves empty input under no-op replacement rules.
testReplaceEmptyInputIdentity :: TestCase
testReplaceEmptyInputIdentity =
  mkTestCase "replace preserves identity on empty input under no-op rules" $
    assertEq "replace should keep input unchanged when old is empty" "" $
      replace "" "" ""

-- Confirms replace performs substitution across all non-overlapping matches.
testReplaceAllMatches :: TestCase
testReplaceAllMatches =
  mkTestCase "replace applies substitution consistently across the whole input" $
    assertEq "replace should replace all non-overlapping matches" "x b x" $
      replace "aa" "x" "aa b aa"

-- Confirms replace supports deleting matched text with an empty replacement.
testReplaceDeletion :: TestCase
testReplaceDeletion =
  mkTestCase "replace supports deletion semantics via empty replacement" $
    assertEq "replace should replace all non-overlapping matches" " b " $
      replace "aa" "" "aa b aa"

-- Confirms replace leaves content unchanged when there are no matches.
testReplaceNoMatch :: TestCase
testReplaceNoMatch =
  mkTestCase "replace is non-destructive when no match exists" $
    assertEq "replace should leave unmatched content intact" "hello world" $
      replace "zzz" "x" "hello world"

-- Confirms replace keeps empty input stable when nothing can match.
testReplaceNoMatchOnEmptyInput :: TestCase
testReplaceNoMatchOnEmptyInput =
  mkTestCase "replace keeps empty input stable when there is nothing to match" $
    assertEq "replace should leave unmatched content intact" "" $
      replace "zzz" "x" ""

-- Confirms trim collapses whitespace-only input to the canonical empty string.
testTrimWhitespaceOnly :: TestCase
testTrimWhitespaceOnly =
  mkTestCase "trim normalizes whitespace-only input to canonical empty output" $
    assertEq "trim should remove leading and trailing whitespace" "" $
      trim " \n\t  \t "

-- Confirms trim removes surrounding whitespace while preserving core token text.
testTrimBoundaryWhitespace :: TestCase
testTrimBoundaryWhitespace =
  mkTestCase "trim removes boundary noise while preserving the core token" $
    assertEq "trim should remove leading and trailing whitespace" "hello" $
      trim " \n\t hello \t "

-- Confirms trim keeps meaningful internal spacing untouched.
testTrimKeepsInnerWhitespace :: TestCase
testTrimKeepsInnerWhitespace =
  mkTestCase "trim preserves meaningful internal spacing" $
    assertEq "trim should keep inner whitespace" "a  b" $
      trim "  a  b  "
