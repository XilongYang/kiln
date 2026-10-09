module Kiln.StrSpec (spec) where

import Kiln.Str (replaceAll, trim)
import Test.Hspec

spec :: Spec
spec = do
  describe "trim" $ do
    it "removes leading and trailing whitespace" $
      trim "  hello world  " `shouldBe` "hello world"

    it "leaves internal whitespace untouched" $
      trim "  a  b  " `shouldBe` "a  b"

    it "returns an empty string when the input is all whitespace" $
      trim "   \t\n  " `shouldBe` ""

    it "leaves a string with no surrounding whitespace unchanged" $
      trim "hello" `shouldBe` "hello"

  describe "replaceAll" $ do
    it "replaces every occurrence of the needle" $
      replaceAll "ab" "X" "ababab" `shouldBe` "XXX"

    it "returns the input unchanged when the needle is absent" $
      replaceAll "zz" "X" "hello" `shouldBe` "hello"
