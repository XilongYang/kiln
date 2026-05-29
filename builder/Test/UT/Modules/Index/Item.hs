module Test.UT.Modules.Index.Item (suiteName, testCases) where

import Modules.Index.Item
import Modules.Post (Post(..), PostMeta(..))
import Test.Framework.Asserts
import Test.Framework.TestSuite

suiteName :: String
suiteName = "IndexItem"

testCases :: [TestCase]
testCases =
  [ testMkIndexItemMapsMetaAndUrl
  , testMkIndexItemKeepsDateStringSlices
  ]

testMkIndexItemMapsMetaAndUrl :: TestCase
testMkIndexItemMapsMetaAndUrl =
  mkTestCase "mkIndexItem maps title/date/url from post payload" $ do
    let post = mkPost "my-post" "2026-03-22" "Fixture Title"
    let item = mkIndexItem post
    assertEq "title should come from meta title" "Fixture Title" (itemTitle item)
    assertEq "year should be parsed from date" "2026" (itemYear item)
    assertEq "month should be parsed from date" "03" (itemMonth item)
    assertEq "day should be parsed from date" "22" (itemDay item)
    assertEq "url should be generated under /post/" "/post/my-post.html" (itemUrl item)

testMkIndexItemKeepsDateStringSlices :: TestCase
testMkIndexItemKeepsDateStringSlices =
  mkTestCase "mkIndexItem keeps raw string slices for month/day fields" $ do
    let post = mkPost "edge-post" "2031-12-01" "Edge"
    let item = mkIndexItem post
    assertEq "month should preserve zero padding as raw substring" "12" (itemMonth item)
    assertEq "day should preserve zero padding as raw substring" "01" (itemDay item)

mkPost :: String -> String -> String -> Post
mkPost name date title =
  Post
    { postName = name
    , postSourcePath = "src" ++ name ++ ".md"
    , postBody = ""
    , postAbstract = ""
    , postMeta = PostMeta title "author" date
    }
