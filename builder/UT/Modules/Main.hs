module UT.Modules.Main (suiteName, testCases) where

import Control.Exception (bracket_)
import Modules.Config
import Modules.Utils.TempDir (withTempDir)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , setCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "Main"

testCases :: [TestCase]
testCases =
  [ testMainBuildsCoreOutputs
  ]

testMainBuildsCoreOutputs :: TestCase
testMainBuildsCoreOutputs =
  mkTestCase "main builds post/index/searchdb/font outputs in isolated workspace" $ do
    repoRoot <- getCurrentDirectory
    tempRoot <- getTemporaryDirectory
    let workRoot = tempRoot </> "xilong-site-ut-main"
        builderMainPath = repoRoot </> "builder" </> "Main.hs"
        builderIncludePath = repoRoot </> "builder"
    withTempDir workRoot $ do
      let binDir = workRoot </> "bin"
      createDirectoryIfMissing True binDir
      writeFakePyftsubset (binDir </> "pyftsubset")
      withWorkDir workRoot $ do
        setupFixtureTree
        withPrependedPath binDir $
          callProcess "runghc"
            [ "-i" ++ builderIncludePath
            , builderMainPath
            ]
      postExists <- doesFileExist (workRoot </> postPath </> "fixture.html")
      indexExists <- doesFileExist (workRoot </> indexPath)
      searchExists <- doesFileExist (workRoot </> searchDBPath)
      subsetExists <- doesFileExist (workRoot </> subsetFontFilePath)
      assertTrue "main should render one post html output" postExists
      assertTrue "main should render index.html output" indexExists
      assertTrue "main should generate searchdb.json output" searchExists
      assertTrue "main should generate subset font output" subsetExists

setupFixtureTree :: IO ()
setupFixtureTree = do
  createDirectoryIfMissing True srcPath
  createDirectoryIfMissing True templateComponentPath
  createDirectoryIfMissing True fontPath
  writeFile (srcPath </> "fixture.md") fixturePostMarkdown
  writeFile templatePostPath fixturePostTemplate
  writeFile templateIndexPath fixtureIndexTemplate
  writeFile originFontFilePath "fake-otf"

fixturePostMarkdown :: String
fixturePostMarkdown =
  unlines
    [ "---"
    , "title: Fixture Title"
    , "author: Fixture Author"
    , "date: 2026-03-25"
    , "---"
    , ""
    , "This is abstract."
    , ""
    , "## Section A"
    , ""
    , "Body line."
    ]

fixturePostTemplate :: String
fixturePostTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body><main>$body$</main></body></html>"
    ]

fixtureIndexTemplate :: String
fixtureIndexTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body>$posts$</body></html>"
    ]

writeFakePyftsubset :: FilePath -> IO ()
writeFakePyftsubset scriptPath = do
  writeFile scriptPath $
    unlines
      [ "#!/bin/sh"
      , "for arg in \"$@\"; do"
      , "  case \"$arg\" in"
      , "    --output-file=*) out=\"${arg#--output-file=}\" ;;"
      , "  esac"
      , "done"
      , "[ -n \"$out\" ] && : > \"$out\""
      ]
  callProcess "chmod" ["+x", scriptPath]

withWorkDir :: FilePath -> IO a -> IO a
withWorkDir dir action = do
  old <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory old) action

withPrependedPath :: FilePath -> IO a -> IO a
withPrependedPath path action = do
  old <- lookupEnv "PATH"
  let newPath = case old of
        Just existing -> path ++ ":" ++ existing
        Nothing -> path
  bracket_ (setEnv "PATH" newPath) (restore old) action
  where
    restore (Just value) = setEnv "PATH" value
    restore Nothing = unsetEnv "PATH"
