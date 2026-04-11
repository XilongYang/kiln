module Test.UT.Modules.SearchDB (suiteName, testCases) where

import Modules.SearchDB
  ( SearchItem(..)
  )
import Modules.Utils.Klb (parseKlb, renderKlb)
import Test.Framework.Asserts
import Test.Framework.Expect (expectRight)
import Test.Framework.TestSuite

-- Suite for search item KLB serialization helpers.
suiteName :: String
suiteName = "SearchDB"

testCases :: [TestCase]
testCases =
  [ testSearchItemRoundTripViaKlb
  , testSearchItemRenderNormalizesWhitespace
  ]

-- Confirms SearchItem can be encoded and decoded as one KLB block.
testSearchItemRoundTripViaKlb :: TestCase
testSearchItemRoundTripViaKlb =
  mkTestCase "SearchItem round-trips through renderKlb/parseKlb" $ do
    let original = SearchItem "Fixture Title" "/post/fixture.html" "fixture plain body"
    klb <- expectRight "renderKlb should encode SearchItem block" (renderKlb [original])
    decoded <- expectRight "parseKlb should decode SearchItem block" (parseKlb klb)
    assertEq "decoded search item should equal original value"
      [original]
      (decoded :: [SearchItem])

-- Confirms KLB render normalization still flattens whitespace in search content.
testSearchItemRenderNormalizesWhitespace :: TestCase
testSearchItemRenderNormalizesWhitespace =
  mkTestCase "renderKlb normalizes search-item content into single-line text" $ do
    let item = SearchItem "Fixture Title" "/post/fixture.html" "line1\nline2\tline3"
    klb <- expectRight "renderKlb should succeed for SearchItem content normalization" (renderKlb [item])
    parsed <- expectRight "parseKlb should succeed on rendered SearchItem KLB" (parseKlb klb)
    let [decoded] = (parsed :: [SearchItem])
    assertFalse "normalized search item content should not contain newlines"
      ('\n' `elem` searchItemContent decoded)
    assertEq "whitespace should be collapsed to single spaces by KLB rendering"
      "line1 line2 line3"
      (searchItemContent decoded)
