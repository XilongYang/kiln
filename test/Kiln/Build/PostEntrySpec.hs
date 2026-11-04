module Kiln.Build.PostEntrySpec (spec) where

import Kiln.Build.PostEntry (PostFrontmatter (..), parseFrontmatter)
import Test.Hspec

-- | What pandoc writes for a post, as `frontmatterTemplate` shapes it.
rendered :: String -> String -> String -> String
rendered title date tags =
  "kiln-title: " ++ title ++ "\nkiln-date: " ++ date ++ "\nkiln-tags: " ++ tags ++ "\n"

spec :: Spec
spec = describe "parseFrontmatter" $ do
  it "reads a post's title, date and tags back" $
    parseFrontmatter (rendered "A Title" "2024-03-15" "long,image")
      `shouldBe` PostFrontmatter "A Title" "2024-03-15" "long,image"

  -- Characters JSON would need escaping; labelled lines need none.
  it "reads a title carrying straight quotes" $
    fmTitle (parseFrontmatter (rendered "Using \"quotes\" in code" "2024-03-15" ""))
      `shouldBe` "Using \"quotes\" in code"

  it "reads a title carrying a backslash" $
    fmTitle (parseFrontmatter (rendered "C:\\path" "2024-03-15" ""))
      `shouldBe` "C:\\path"

  it "keeps the labelled line of a title pandoc wrapped onto several" $
    fmTitle (parseFrontmatter "kiln-title: line one\n\nkiln-date: 2024-03-15\nkiln-tags: \n")
      `shouldBe` "line one"

  it "still finds the fields that follow a multi-line value" $
    fmDate (parseFrontmatter "kiln-title: line one\n\nkiln-date: 2024-03-15\nkiln-tags: \n")
      `shouldBe` "2024-03-15"

  it "reads an unset field as empty" $
    parseFrontmatter (rendered "A Title" "2024-03-15" "")
      `shouldBe` PostFrontmatter "A Title" "2024-03-15" ""

  it "reads a missing label as empty, rather than failing" $
    parseFrontmatter "kiln-date: 2024-03-15\n"
      `shouldBe` PostFrontmatter "" "2024-03-15" ""
