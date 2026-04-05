module Test.UT.Modules.Utils.Klb (suiteName, testCases) where

import Modules.Utils.Klb
import Test.Framework.Asserts
import Test.Framework.TestSuite

newtype EchoBlock = EchoBlock {unEchoBlock :: KlbBlock}
  deriving (Eq, Show)

instance Klb EchoBlock where
  toKlbBlock = unEchoBlock
  fromKlbBlock = Right . EchoBlock

suiteName :: String
suiteName = "Utils.Klb"

testCases :: [TestCase]
testCases =
  [ testRenderKlbEmptyInput
  , testRenderKlbRejectsEmptyBlock
  , testRenderKlbRejectsMissingKey
  , testRenderKlbRejectsColonInKey
  , testRenderKlbNormalizesWhitespace
  , testParseKlbEmptyInput
  , testParseKlbParsesMultipleBlocks
  , testParseKlbRejectsInvalidLine
  , testParseKlbRejectsInvalidHeaderKey
  , testParseKlbRejectsInvalidSizeValue
  , testParseKlbRejectsNonPositiveSize
  , testParseKlbRejectsIncompleteBlock
  ]

testRenderKlbEmptyInput :: TestCase
testRenderKlbEmptyInput =
  mkTestCase "renderKlb returns empty output for empty list input" $
    assertEq "renderKlb should encode empty object list as empty text" (Right "") $
      renderKlb ([] :: [EchoBlock])

testRenderKlbRejectsEmptyBlock :: TestCase
testRenderKlbRejectsEmptyBlock =
  mkTestCase "renderKlb rejects blocks without fields" $
    assertEq "renderKlb should fail when one block has no fields"
      "Left EmptyBlock"
      (show (renderKlb [EchoBlock []]))

testRenderKlbRejectsMissingKey :: TestCase
testRenderKlbRejectsMissingKey =
  mkTestCase "renderKlb rejects empty field keys" $
    assertEq "renderKlb should fail when key is empty"
      "Left (MissingKey (\"\",\"value\"))"
      (show (renderKlb [EchoBlock [("", "value")]]))

testRenderKlbRejectsColonInKey :: TestCase
testRenderKlbRejectsColonInKey =
  mkTestCase "renderKlb rejects keys that contain a colon" $
    assertEq "renderKlb should fail when key contains ':'"
      "Left (InvalidKey \"a:b\")"
      (show (renderKlb [EchoBlock [("a:b", "v")]]))

testRenderKlbNormalizesWhitespace :: TestCase
testRenderKlbNormalizesWhitespace =
  mkTestCase "renderKlb trims and flattens key/value whitespace" $
    assertEq "renderKlb should normalize surrounding and repeated whitespace"
      (Right "size:1\nk 1:v 1\n")
      (renderKlb [EchoBlock [(" \t k 1 \n", " v\t  1 \n")]])

testParseKlbEmptyInput :: TestCase
testParseKlbEmptyInput =
  mkTestCase "parseKlb returns empty block list for empty text" $
    assertEq "parseKlb should decode empty text as empty list" (Right []) $
      (parseKlb "" :: Either KlbParseError [EchoBlock])

testParseKlbParsesMultipleBlocks :: TestCase
testParseKlbParsesMultipleBlocks =
  mkTestCase "parseKlb parses consecutive blocks based on size headers" $
    assertEq "parseKlb should split and decode multiple blocks"
      (Right [EchoBlock [("a", "1"), ("b", "x:y")], EchoBlock [("c", "3")]])
      (parseKlb (unlines ["size:2", "a:1", "b:x:y", "size:1", "c:3"]) :: Either KlbParseError [EchoBlock])

testParseKlbRejectsInvalidLine :: TestCase
testParseKlbRejectsInvalidLine =
  mkTestCase "parseKlb rejects malformed key-value lines without ':'" $
    assertEq "parseKlb should fail when header line is malformed"
      "Left (InvalidLine \"size\")"
      (show (parseKlb "size\nk:v\n" :: Either KlbParseError [EchoBlock]))

testParseKlbRejectsInvalidHeaderKey :: TestCase
testParseKlbRejectsInvalidHeaderKey =
  mkTestCase "parseKlb rejects header keys other than size" $
    assertEq "parseKlb should fail when header key is not size"
      "Left (InvalidHeaderKey \"count\")"
      (show (parseKlb "count:1\na:1\n" :: Either KlbParseError [EchoBlock]))

testParseKlbRejectsInvalidSizeValue :: TestCase
testParseKlbRejectsInvalidSizeValue =
  mkTestCase "parseKlb rejects non-integer size values" $
    assertEq "parseKlb should fail when size value is not an integer"
      "Left (InvalidSizeValue \"abc\")"
      (show (parseKlb "size:abc\na:1\n" :: Either KlbParseError [EchoBlock]))

testParseKlbRejectsNonPositiveSize :: TestCase
testParseKlbRejectsNonPositiveSize =
  mkTestCase "parseKlb rejects zero-sized blocks (boundary)" $
    assertEq "parseKlb should fail when size is zero"
      "Left (InvalidSize 0)"
      (show (parseKlb "size:0\n" :: Either KlbParseError [EchoBlock]))

testParseKlbRejectsIncompleteBlock :: TestCase
testParseKlbRejectsIncompleteBlock =
  mkTestCase "parseKlb rejects blocks shorter than declared size" $
    assertEq "parseKlb should fail when payload lines are fewer than header size"
      "Left (IncompleteBlock 2 1)"
      (show (parseKlb "size:2\na:1\n" :: Either KlbParseError [EchoBlock]))
