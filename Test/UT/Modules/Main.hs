module Test.UT.Modules.Main (suiteName, testCases) where

import Modules.Config
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Framework.Asserts
import Test.Framework.Fixtures
  ( setupMainFixtureTree
  , writeFakePyftsubsetSimple
  )
import Test.Framework.Paths
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Main"

testCases :: [TestCase]
testCases =
  [ testMainBuildsCoreOutputs
  ]

testMainBuildsCoreOutputs :: TestCase
testMainBuildsCoreOutputs =
  mkTestCase "main builds post/index/search/font outputs in isolated workspace" $ do
    withCasePathsInSandbox suiteName "mainBuildsCoreOutputs" [] $ \casePaths -> do
      let workRoot = caseRootDir casePaths
          repoRoot = repoRootPath
          builderMainPath = repoRoot </> "Src" </> "Main.hs"
          builderSourceIncludePath = repoRoot </> "Src"
          builderTestIncludePath = repoRoot
          binDir = workRoot </> "bin"
      createDirectoryIfMissing True binDir
      writeFakePyftsubsetSimple (binDir </> "pyftsubset")
      setupMainFixtureTree
      withPrependedPath binDir $
        callProcess "runghc"
          [ "-i" ++ builderSourceIncludePath
          , "-i" ++ builderTestIncludePath
          , builderMainPath
          ]
      postExists <- doesFileExist (postPath </> "fixture.html")
      indexExists <- doesFileExist indexPath
      searchDbExists <- doesFileExist searchDBPath
      subsetFontExists <- doesFileExist subsetFontFilePath
      metaExists <- doesFileExist (metaArtifactsPath </> "fixture.klb")
      searchItemExists <- doesFileExist (searchItemArtifactsPath </> "fixture.klb")
      charsetExists <- doesFileExist (charsetArtifactsPath </> "fixture.txt")
      assertTrue "main should render one post html output" postExists
      assertTrue "main should render index.html output" indexExists
      assertTrue "main should create searchdb output from search-item artifacts" searchDbExists
      assertTrue "main should generate subset font file" subsetFontExists
      assertTrue "main should cache per-post metadata artifact" metaExists
      assertTrue "main should cache per-post search-item artifact" searchItemExists
      assertTrue "main should cache per-post charset artifact" charsetExists
      searchDb <- readFile searchDBPath
      assertContains "searchdb should include serialized fixture title in KLB payload"
        "searchItemTitle:Fixture Title"
        searchDb
