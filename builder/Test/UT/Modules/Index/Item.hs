module Test.UT.Modules.Index.Item (suiteName, testCases) where

import Modules.Index.Item
import Modules.Post (PostMeta(..))
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
  mkTestCase "mkIndexItem maps title/date/url from post metadata" $ do
    let meta = PostMeta "Fixture Title" "author" "2026-03-22"
    let item = mkIndexItem meta "/post/my-post.html"
    assertEq "title should come from meta title" "Fixture Title" (itemTitle item)
    assertEq "year should be parsed from date" "2026" (itemYear item)
    assertEq "month should be parsed from date" "03" (itemMonth item)
    assertEq "day should be parsed from date" "22" (itemDay item)
    assertEq "url should come from provided argument" "/post/my-post.html" (itemUrl item)

testMkIndexItemKeepsDateStringSlices :: TestCase
testMkIndexItemKeepsDateStringSlices =
  mkTestCase "mkIndexItem keeps raw string slices for month/day fields" $ do
    let meta = PostMeta "Edge" "author" "2031-12-01"
    let item = mkIndexItem meta "/post/edge-post.html"
    assertEq "month should preserve zero padding as raw substring" "12" (itemMonth item)
    assertEq "day should preserve zero padding as raw substring" "01" (itemDay item)
