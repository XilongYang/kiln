module UT.Modules.Template (suiteName, testCases) where

import Modules.Config
import Modules.Template
import UT.TestUtils.TestSuite
import UT.TestUtils.Asserts

import Data.List (isInfixOf)

-- Suite for placeholder expansion and template/component loading.
suiteName :: String
suiteName = "Template"

testCases :: [TestCase]
testCases =
  [ -- Test Case#1: Pure placeholder expansion should replace each known component.
    mkTestCase "expandComponents replaces declared placeholders" $ do
      let html = "<!--<navbar>--><main><!--<footnotes>--></main>"
      let components = [("navbar", "<nav>bar</nav>"), ("footnotes", "<footer>end</footer>")]
      let expanded = expandComponents html components
      assertEq "expandComponents should replace every declared placeholder"
        "<nav>bar</nav><main><footer>end</footer></main>"
        expanded
  , -- Test Case#2: Component loader should derive the name from the filename and read its html.
    mkTestCase "loadComponent reads html and derives component name" $ do
      (name, componentHtml) <- loadComponent "navbar.html"
      assertEq "loadComponent should use filename stem as component name" "navbar" name
      assertContains "loadComponent should read component html" "darkmode" componentHtml
  , -- Test Case#3: Full template generation should inline component html and remove placeholders.
    mkTestCase "genTemplate expands component placeholders in template files" $ do
      generated <- genTemplate templatePostPath
      assertContains "genTemplate should include inserted common_head html" "viewport" generated
      assertContains "genTemplate should include inserted navbar html" "darkmode" generated
      assertFalse "genTemplate should remove component placeholders for navbar" $
        "<!--<navbar>-->" `isInfixOf` generated
  ]
