module UT.Modules.IndexBuilder (suiteName, testCases) where

import Data.List (isPrefixOf)
import Modules.Builder.IndexBuilder
import Modules.IndexItem (IndexItem(..))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for index-builder grouping and ordering helpers.
suiteName :: String
suiteName = "IndexBuilder"

testCases :: [TestCase]
testCases =
  [ testGenIndexHtmlGroupsAndSortsByDate
  ]

-- Confirms index html groups by year and sorts entries by descending date per year.
testGenIndexHtmlGroupsAndSortsByDate :: TestCase
testGenIndexHtmlGroupsAndSortsByDate =
  mkTestCase "genIndexHtml groups items by year and orders dates descending in a year" $ do
    let items =
          [ IndexItem "B-Old" "2025" "01" "02" "/post/b-old.html"
          , IndexItem "A-New" "2026" "12" "25" "/post/a-new.html"
          , IndexItem "A-Old" "2026" "02" "03" "/post/a-old.html"
          ]
    let rendered = genIndexHtml items "<main>$posts$</main>"
    assertContains "index html should include 2026 year heading" "<h3>2026</h3>" rendered
    assertContains "index html should include 2025 year heading" "<h3>2025</h3>" rendered
    assertTrue "newer year group should appear before older year group"
      (indexOf "<h3>2026</h3>" rendered < indexOf "<h3>2025</h3>" rendered)
    assertTrue "items in same year should be ordered by date descending"
      (indexOf "A-New" rendered < indexOf "A-Old" rendered)

indexOf :: String -> String -> Int
indexOf needle haystack = go 0 haystack
  where
    go n rest
      | needle `isPrefixOf` rest = n
      | null rest = maxBound
      | otherwise = go (n + 1) (drop 1 rest)
