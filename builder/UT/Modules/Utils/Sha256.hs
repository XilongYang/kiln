module UT.Modules.Utils.Sha256 (suiteName, testCases) where

import Data.List (all)
import Data.Word (Word8)
import Modules.Utils.Sha256
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "Utils.Sha256"

testCases :: [TestCase]
testCases =
  [ testSha256HexEmptyVector
  , testSha256HexAbcVector
  , testSha256HexHelloWorldVector
  , testSha256HexNonAsciiVector
  ]

testSha256HexEmptyVector :: TestCase
testSha256HexEmptyVector =
  mkTestCase "sha256Hex matches empty-string test vector" $
    assertEq "sha256Hex of empty string should match known vector"
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      (sha256Hex "")

testSha256HexAbcVector :: TestCase
testSha256HexAbcVector =
  mkTestCase "sha256Hex matches abc test vector" $
    assertEq "sha256Hex of abc should match known vector"
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      (sha256Hex "abc")

testSha256HexHelloWorldVector :: TestCase
testSha256HexHelloWorldVector =
  mkTestCase "sha256Hex matches hello world test vector" $
    assertEq "sha256Hex of hello world should match known vector"
      "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
      (sha256Hex "hello world")

testSha256HexNonAsciiVector :: TestCase
testSha256HexNonAsciiVector =
  mkTestCase "sha256Hex matches non-ASCII test vector" $
    assertEq "sha256Hex of non-ASCII should match known vector"
      "988f34d8fb561d5b7f797c8e6eea9f72372bd780ab1244132300e73253ed88b3"
      (sha256Hex "你好，世界！こんにちは、世界！😃")
