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
  mkTestCase "main builds post and index outputs in isolated workspace" $ do
    withCasePathsInSandbox suiteName "mainBuildsCoreOutputs" [] $ \casePaths -> do
      let workRoot = caseRootDir casePaths
          repoRoot = repoRootPath
          builderMainPath = repoRoot </> "builder" </> "Src" </> "Main.hs"
          builderSourceIncludePath = repoRoot </> "builder" </> "Src"
          builderTestIncludePath = repoRoot </> "builder"
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
      assertTrue "main should render one post html output" postExists
      assertTrue "main should render index.html output" indexExists
