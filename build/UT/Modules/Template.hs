module UT.Modules.Template (suiteName, testCases) where

import Modules.Template
import UT.TestUtils.TestSuite
import UT.TestUtils.Asserts

import Data.List (isInfixOf)
import System.FilePath ((</>))

fixtureRoot :: FilePath
fixtureRoot = "build/UT/.mock/template"

fixtureComponentDir :: FilePath
fixtureComponentDir = fixtureRoot </> "component"

fixtureTemplatePath :: FilePath
fixtureTemplatePath = fixtureRoot </> "post.html"

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
      (name, componentHtml) <- loadComponentFrom fixtureComponentDir "navbar.html"
      assertEq "loadComponent should use filename stem as component name" "navbar" name
      assertContains "loadComponent should read component html" "Fixture Navbar" componentHtml
  , -- Test Case#3: Full template generation should inline component html and remove placeholders.
    mkTestCase "genTemplate expands component placeholders in template files" $ do
      generated <- genTemplateFrom fixtureComponentDir fixtureTemplatePath
      assertContains "genTemplate should include inserted common_head html" "fixture-head" generated
      assertContains "genTemplate should include inserted navbar html" "Fixture Navbar" generated
      assertFalse "genTemplate should remove component placeholders for navbar" $
        "<!--<navbar>-->" `isInfixOf` generated
  ]
