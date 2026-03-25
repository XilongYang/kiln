module UT.Modules.Config (suiteName, testCases) where

import Modules.Config
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "Config"

testCases :: [TestCase]
testCases =
  [ testProjectPathConstants
  , testTemplatePathConstants
  , testWebPathConstants
  , testFontPathConstants
  ]

testProjectPathConstants :: TestCase
testProjectPathConstants =
  mkTestCase "project path constants are composed from rootPath" $ do
    assertEq "rootPath should stay as repository root marker" "." rootPath
    assertEq "srcPath should resolve under root" (rootPath </> "src") srcPath
    assertEq "postPath should resolve under root" (rootPath </> "post") postPath
    assertEq "tempPath should resolve under root" (rootPath </> "temp") tempPath
    assertEq "indexPath should resolve under root" (rootPath </> "index.html") indexPath
    assertEq "searchDBPath should resolve under root" (rootPath </> "searchdb.json") searchDBPath

testTemplatePathConstants :: TestCase
testTemplatePathConstants =
  mkTestCase "template path constants are wired to template root and temp outputs" $ do
    assertEq "templatePath should resolve under root" (rootPath </> "template") templatePath
    assertEq "templatePostPath should point to template/post.html"
      (templatePath </> "post.html")
      templatePostPath
    assertEq "templateIndexPath should point to template/index.html"
      (templatePath </> "index.html")
      templateIndexPath
    assertEq "templateComponentPath should point to template/component"
      (templatePath </> "component")
      templateComponentPath
    assertEq "renderedTemplatePostPath should live in temp/post.html"
      (tempPath </> "post.html")
      renderedTemplatePostPath
    assertEq "renderedTemplateIndexPath should live in temp/index.html"
      (tempPath </> "index.html")
      renderedTemplateIndexPath

testWebPathConstants :: TestCase
testWebPathConstants =
  mkTestCase "web path constants remain canonical site-root URLs" $ do
    assertEq "webRoot should stay root slash" "/" webRoot
    assertEq "webPostPath should be root-relative /post/" "/post/" webPostPath
    assertEq "component token should keep placeholder replacement token" "name" componentPlaceholderToken
    assertEq "component placeholder pattern should contain replacement token"
      "<!--<name>-->"
      componentPlaceholderPattern

testFontPathConstants :: TestCase
testFontPathConstants =
  mkTestCase "font output/input constants point to expected filenames" $ do
    assertEq "fontSetPath should live under temp/fontset.txt"
      (tempPath </> "fontset.txt")
      fontSetPath
    assertEq "fontPath should be root/res/fonts"
      (rootPath </> "res" </> "fonts")
      fontPath
    assertEq "origin font path should point to SourceHanSerifCN-Regular.otf"
      (fontPath </> "SourceHanSerifCN-Regular.otf")
      originFontFilePath
    assertEq "subset font path should point to SourceHanSerifCN-Subset.woff2"
      (fontPath </> "SourceHanSerifCN-Subset.woff2")
      subsetFontFilePath
