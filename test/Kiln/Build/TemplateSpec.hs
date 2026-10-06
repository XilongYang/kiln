module Kiln.Build.TemplateSpec (spec) where

import Kiln.Build.Template (renderComponents, replaceAll)
import Test.Hspec

spec :: Spec
spec = do
  describe "renderComponents" $ do
    it "replaces a single placeholder with its component content" $
      renderComponents [("navbar", "<nav>NAV</nav>")] "<body><!--<navbar>--></body>"
        `shouldBe` "<body><nav>NAV</nav></body>"

    it "replaces multiple distinct placeholders" $
      renderComponents
        [("navbar", "NAV"), ("footer", "FOOT")]
        "<!--<navbar>-->middle<!--<footer>-->"
        `shouldBe` "NAVmiddleFOOT"

    it "leaves the page untouched when no placeholders match" $
      renderComponents [("navbar", "NAV")] "<body>no placeholders here</body>"
        `shouldBe` "<body>no placeholders here</body>"

    it "leaves placeholders with no matching component untouched" $
      renderComponents [("navbar", "NAV")] "<!--<footer>-->"
        `shouldBe` "<!--<footer>-->"

  describe "replaceAll" $ do
    it "replaces every occurrence of the needle" $
      replaceAll "ab" "X" "ababab" `shouldBe` "XXX"

    it "returns the input unchanged when the needle is absent" $
      replaceAll "zz" "X" "hello" `shouldBe` "hello"
