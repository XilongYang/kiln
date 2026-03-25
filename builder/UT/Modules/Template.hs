module UT.Modules.Template (suiteName, testCases) where

import Data.List (isInfixOf)
import Modules.Template

import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  )
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

prepareTemplateFixtures :: CasePaths -> IO (FilePath, FilePath)
prepareTemplateFixtures casePaths = do
  let componentDir = caseTemplateDir casePaths </> "component"
      templatePath = templateFile casePaths "post.html"
  createDirectoryIfMissing True componentDir
  copyFile commonHeadFixturePath (componentDir </> "common_head.html")
  copyFile navbarFixturePath (componentDir </> "navbar.html")
  copyFile postTemplateFixturePath templatePath
  pure (componentDir, templatePath)

-- Suite for placeholder expansion and template/component loading.
suiteName :: String
suiteName = "Template"

testCases :: [TestCase]
testCases =
  [ testExpandComponentsReplacesDeclaredPlaceholders
  , testLoadComponentReadsNameAndHtml
  , testGenTemplateExpandsAndRemovesPlaceholders
  , testExpandComponentsKeepsUnknownPlaceholders
  , testExpandComponentsWithEmptyListNoChange
  , testLoadComponentsFromFiltersAndSorts
  , testLoadComponentFromMissingFileThrows
  , testLoadComponentsFromMissingDirThrows
  , testGenTemplateFromMissingTemplateThrows
  ]

-- Confirms known placeholders are replaced by matching component html.
testExpandComponentsReplacesDeclaredPlaceholders :: TestCase
testExpandComponentsReplacesDeclaredPlaceholders =
  mkTestCase "expandComponents replaces declared placeholders" $ do
    let html = "<!--<navbar>--><main><!--<footnotes>--></main>"
    let components = [("navbar", "<nav>bar</nav>"), ("footnotes", "<footer>end</footer>")]
    let expanded = expandComponents html components
    assertEq "expandComponents should replace every declared placeholder"
      "<nav>bar</nav><main><footer>end</footer></main>"
      expanded

-- Confirms component loader derives component name from filename and reads html.
testLoadComponentReadsNameAndHtml :: TestCase
testLoadComponentReadsNameAndHtml =
  mkTestCase "loadComponent reads html and derives component name" $
    withCasePaths suiteName "loadComponentReadsNameAndHtml" ["template"] $ \casePaths -> do
      (componentDir, _) <- prepareTemplateFixtures casePaths
      (name, componentHtml) <- loadComponentFrom componentDir "navbar.html"
      assertEq "loadComponent should use filename stem as component name" "navbar" name
      assertContains "loadComponent should read component html" "Fixture Navbar" componentHtml

-- Confirms template generation inlines component html and removes placeholders.
testGenTemplateExpandsAndRemovesPlaceholders :: TestCase
testGenTemplateExpandsAndRemovesPlaceholders =
  mkTestCase "genTemplate expands component placeholders in template files" $
    withCasePaths suiteName "genTemplateExpandsAndRemovesPlaceholders" ["template"] $ \casePaths -> do
      (componentDir, templatePath) <- prepareTemplateFixtures casePaths
      generated <- genTemplateFrom componentDir templatePath
      assertContains "genTemplate should include inserted common_head html" "fixture-head" generated
      assertContains "genTemplate should include inserted navbar html" "Fixture Navbar" generated
      assertFalse "genTemplate should remove component placeholders for navbar" $
        "<!--<navbar>-->" `isInfixOf` generated

-- Confirms unknown placeholders remain untouched during expansion.
testExpandComponentsKeepsUnknownPlaceholders :: TestCase
testExpandComponentsKeepsUnknownPlaceholders =
  mkTestCase "expandComponents keeps unknown placeholders unchanged" $ do
    let html = "<!--<missing>--><section>ok</section>"
    let expanded = expandComponents html [("navbar", "<nav>bar</nav>")]
    assertEq "expandComponents should not remove unknown placeholders"
      "<!--<missing>--><section>ok</section>"
      expanded

-- Confirms empty component list leaves template html unchanged.
testExpandComponentsWithEmptyListNoChange :: TestCase
testExpandComponentsWithEmptyListNoChange =
  mkTestCase "expandComponents with empty list keeps original html" $ do
    let html = "<!--<navbar>--><article>body</article>"
    let expanded = expandComponents html []
    assertEq "expandComponents should return original html when no components exist"
      html
      expanded

-- Confirms component directory loader keeps only lowercase .html files in sorted order.
testLoadComponentsFromFiltersAndSorts :: TestCase
testLoadComponentsFromFiltersAndSorts =
  mkTestCase "loadComponentsFrom filters non-html files and sorts by filename" $
    withCasePaths suiteName "loadComponentsFromFiltersAndSorts" ["template"] $ \casePaths -> do
      let dir = caseTemplateDir casePaths </> "component-sort"
      createDirectoryIfMissing True dir
      writeFile (dir </> "zeta.html") "Z"
      writeFile (dir </> "alpha.html") "A"
      writeFile (dir </> "notes.txt") "ignored"
      writeFile (dir </> "BETA.HTML") "ignored-by-case"

      components <- loadComponentsFrom dir
      assertEq "loadComponentsFrom should load sorted .html components only"
        [("alpha", "A"), ("zeta", "Z")]
        components

-- Confirms loading a missing component file raises an exception.
testLoadComponentFromMissingFileThrows :: TestCase
testLoadComponentFromMissingFileThrows =
  mkTestCase "loadComponentFrom throws when component file does not exist" $
    withCasePaths suiteName "loadComponentFromMissingFileThrows" ["template"] $ \casePaths -> do
      (componentDir, _) <- prepareTemplateFixtures casePaths
      assertThrows "loadComponentFrom should fail on a missing file" $
        loadComponentFrom componentDir "does-not-exist.html"

-- Confirms loading components from a missing directory raises an exception.
testLoadComponentsFromMissingDirThrows :: TestCase
testLoadComponentsFromMissingDirThrows =
  mkTestCase "loadComponentsFrom throws when directory does not exist" $
    withCasePaths suiteName "loadComponentsFromMissingDirThrows" ["template"] $ \casePaths -> do
      assertThrows "loadComponentsFrom should fail on a missing directory" $
        loadComponentsFrom (caseTemplateDir casePaths </> "no-such-dir")

-- Confirms generating template from a missing template file raises an exception.
testGenTemplateFromMissingTemplateThrows :: TestCase
testGenTemplateFromMissingTemplateThrows =
  mkTestCase "genTemplateFrom throws when template file does not exist" $
    withCasePaths suiteName "genTemplateFromMissingTemplateThrows" ["template"] $ \casePaths -> do
      (componentDir, _) <- prepareTemplateFixtures casePaths
      assertThrows "genTemplateFrom should fail on a missing template file" $
        genTemplateFrom componentDir (caseTemplateDir casePaths </> "missing-template.html")
