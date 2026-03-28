module UT.Modules.Toc (suiteName, testCases) where

import Data.List (isInfixOf)
import Modules.Toc (injectToc)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "Toc"

testCases :: [TestCase]
testCases =
  [ testInjectTocScansHeadingsUnderMainOnly
  ]

testInjectTocScansHeadingsUnderMainOnly :: TestCase
testInjectTocScansHeadingsUnderMainOnly =
  mkTestCase "injectToc injects toc and excludes headings before main" $ do
    let html =
          unlines
            [ "<h2 id=\"head-only\">Head Only</h2>"
            , "<main>"
            , "<h2 id=\"main-title\">Main Title</h2>"
            , "<h3 id=\"main-sub\">Main Sub</h3>"
            , "</main>"
            , "[[toc]]"
            ]
    let rendered = injectToc html
    assertContains "toc nav wrapper should be injected" "<nav role=\"navigation\" class=\"toc\">" rendered
    assertContains "main h2 should be present in toc links" "href=\"#main-title\"" rendered
    assertContains "main h3 should be present in toc links" "href=\"#main-sub\"" rendered
    assertFalse "heading before main should be excluded from toc"
      ("href=\"#head-only\"" `isInfixOf` rendered)
