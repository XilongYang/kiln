module Test.UT.Modules.Config (suiteName, testCases) where

import Modules.Config
import System.FilePath ((</>))
import Test.Framework.Asserts
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Config"

testCases :: [TestCase]
testCases =
  [ testProjectPathConstants
  , testTemplatePathConstants
  , testWebPathConstants
  , testFontPathConstants
  , testCachePathConstants
  ]

testProjectPathConstants :: TestCase
testProjectPathConstants =
  mkTestCase "project path constants are composed from rootPath" $ do
    assertEq "rootPath should stay as repository root marker" "." rootPath
    assertEq "srcPath should resolve under root" (rootPath </> "src") srcPath
    assertEq "postPath should resolve under root" (rootPath </> "post") postPath
    assertEq "tempPath should resolve under root" (rootPath </> "temp") tempPath
    assertEq "indexPath should resolve under root" (rootPath </> "index.html") indexPath
    assertEq "searchDBPath should resolve under root" (rootPath </> "searchdb.klb") searchDBPath

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
    assertEq "origin font path should point to cn.woff2"
      (fontPath </> "cn.woff2")
      originFontFilePath
    assertEq "subset font path should point to cn-subset.woff2"
      (fontPath </> "cn-subset.woff2")
      subsetFontFilePath

testCachePathConstants :: TestCase
testCachePathConstants =
  mkTestCase "cache path constants point to expected state/artifact directories" $ do
    assertEq "cacheRoot should resolve under root/.cache"
      (rootPath </> ".cache")
      cacheRoot
    assertEq "cacheStatePath should resolve under .cache/state"
      (cacheRoot </> "state")
      cacheStatePath
    assertEq "builderStatePath should point to builder.state"
      (cacheStatePath </> "builder.state")
      builderStatePath
    assertEq "postTemplateStatePath should point to post-template.state"
      (cacheStatePath </> "post-template.state")
      postTemplateStatePath
    assertEq "indexTemplateStatePath should point to index-template.state"
      (cacheStatePath </> "index-template.state")
      indexTemplateStatePath
    assertEq "postStatePath should point to cache/state/post"
      (cacheStatePath </> "post")
      postStatePath
    assertEq "cacheArtifactsPath should resolve under .cache/artifacts"
      (cacheRoot </> "artifacts")
      cacheArtifactsPath
    assertEq "metaArtifactsPath should point to cache/artifacts/meta"
      (cacheArtifactsPath </> "meta")
      metaArtifactsPath
    assertEq "searchItemArtifactsPath should point to cache/artifacts/search-item"
      (cacheArtifactsPath </> "search-item")
      searchItemArtifactsPath
    assertEq "charsetArtifactsPath should point to cache/artifacts/charset"
      (cacheArtifactsPath </> "charset")
      charsetArtifactsPath
