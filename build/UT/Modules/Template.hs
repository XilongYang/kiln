module UT.Modules.Template (suiteName, testCases) where

import Modules.Template
import UT.TestUtils.TestSuite
import UT.TestUtils.Asserts

import Data.List (isInfixOf)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , removePathForcibly
  )
import System.FilePath ((</>))
import System.IO (writeFile)

fixtureRoot :: FilePath
fixtureRoot = "build/UT/.mock/template"

utComponentDir :: FilePath
utComponentDir = fixtureRoot </> "component"

utTemplatePath :: FilePath
utTemplatePath = fixtureRoot </> "post.html"

utRuntimeRoot :: FilePath
utRuntimeRoot = fixtureRoot </> "runtime"

withFreshRuntimeDir :: FilePath -> IO a -> IO a
withFreshRuntimeDir dir action = do
  exists <- doesDirectoryExist dir
  if exists then removePathForcibly dir else pure ()
  createDirectoryIfMissing True dir
  action

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
      (name, componentHtml) <- loadComponentFrom utComponentDir "navbar.html"
      assertEq "loadComponent should use filename stem as component name" "navbar" name
      assertContains "loadComponent should read component html" "Fixture Navbar" componentHtml
  , -- Test Case#3: Full template generation should inline component html and remove placeholders.
    mkTestCase "genTemplate expands component placeholders in template files" $ do
      generated <- genTemplateFrom utComponentDir utTemplatePath
      assertContains "genTemplate should include inserted common_head html" "fixture-head" generated
      assertContains "genTemplate should include inserted navbar html" "Fixture Navbar" generated
      assertFalse "genTemplate should remove component placeholders for navbar" $
        "<!--<navbar>-->" `isInfixOf` generated
  , -- Test Case#4: Placeholder names not in the component list should be preserved.
    mkTestCase "expandComponents keeps unknown placeholders unchanged" $ do
      let html = "<!--<missing>--><section>ok</section>"
      let expanded = expandComponents html [("navbar", "<nav>bar</nav>")]
      assertEq "expandComponents should not remove unknown placeholders"
        "<!--<missing>--><section>ok</section>"
        expanded
  , -- Test Case#5: Empty component list should leave template as-is.
    mkTestCase "expandComponents with empty list keeps original html" $ do
      let html = "<!--<navbar>--><article>body</article>"
      let expanded = expandComponents html []
      assertEq "expandComponents should return original html when no components exist"
        html
        expanded
  , -- Test Case#6: Bulk component loader should only include .html files and keep sorted order.
    mkTestCase "loadComponentsFrom filters non-html files and sorts by filename" $
      withFreshRuntimeDir (utRuntimeRoot </> "component-sort") $ do
        let dir = utRuntimeRoot </> "component-sort"
        writeFile (dir </> "zeta.html") "Z"
        writeFile (dir </> "alpha.html") "A"
        writeFile (dir </> "notes.txt") "ignored"
        writeFile (dir </> "BETA.HTML") "ignored-by-case"

        components <- loadComponentsFrom dir
        assertEq "loadComponentsFrom should load sorted .html components only"
          [("alpha", "A"), ("zeta", "Z")]
          components
  , -- Test Case#7: Loading a missing component file should throw.
    mkTestCase "loadComponentFrom throws when component file does not exist" $
      assertThrows "loadComponentFrom should fail on a missing file" $
        loadComponentFrom utComponentDir "does-not-exist.html"
  , -- Test Case#8: Loading components from a missing directory should throw.
    mkTestCase "loadComponentsFrom throws when directory does not exist" $
      assertThrows "loadComponentsFrom should fail on a missing directory" $
        loadComponentsFrom (utRuntimeRoot </> "no-such-dir")
  , -- Test Case#9: Generating template from a missing template file should throw.
    mkTestCase "genTemplateFrom throws when template file does not exist" $
      assertThrows "genTemplateFrom should fail on a missing template file" $
        genTemplateFrom utComponentDir (utRuntimeRoot </> "missing-template.html")
  ]
