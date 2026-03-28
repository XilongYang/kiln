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

suiteName :: String
suiteName = "Template"

testCases :: [TestCase]
testCases =
  [ testExpandTemplateExpandsAndRemovesPlaceholders
  , testExpandTemplateWithMissingTemplateThrows
  , testExpandTemplateWithMissingComponentDirThrows
  ]

testExpandTemplateExpandsAndRemovesPlaceholders :: TestCase
testExpandTemplateExpandsAndRemovesPlaceholders =
  mkTestCase "expandTemplate expands component placeholders in template files" $
    withCasePaths suiteName "expandTemplateExpandsAndRemovesPlaceholders" ["template"] $ \casePaths -> do
      (componentDir, templatePath) <- prepareTemplateFixtures casePaths
      generated <- expandTemplate templatePath componentDir
      assertContains "expandTemplate should include inserted common_head html" "fixture-head" generated
      assertContains "expandTemplate should include inserted navbar html" "Fixture Navbar" generated
      assertFalse "expandTemplate should remove component placeholders for navbar" $
        "<!--<navbar>-->" `isInfixOf` generated

testExpandTemplateWithMissingTemplateThrows :: TestCase
testExpandTemplateWithMissingTemplateThrows =
  mkTestCase "expandTemplate throws when template file does not exist" $
    withCasePaths suiteName "expandTemplateWithMissingTemplateThrows" ["template"] $ \casePaths -> do
      (componentDir, _) <- prepareTemplateFixtures casePaths
      assertThrows "expandTemplate should fail on a missing template file" $
        expandTemplate (caseTemplateDir casePaths </> "missing-template.html") componentDir

testExpandTemplateWithMissingComponentDirThrows :: TestCase
testExpandTemplateWithMissingComponentDirThrows =
  mkTestCase "expandTemplate throws when component directory does not exist" $
    withCasePaths suiteName "expandTemplateWithMissingComponentDirThrows" ["template"] $ \casePaths -> do
      (_, templatePath) <- prepareTemplateFixtures casePaths
      assertThrows "expandTemplate should fail on a missing component directory" $
        expandTemplate templatePath (caseTemplateDir casePaths </> "no-such-dir")
