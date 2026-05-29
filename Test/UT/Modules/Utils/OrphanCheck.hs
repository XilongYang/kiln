module Test.UT.Modules.Utils.OrphanCheck (suiteName, testCases) where

import Modules.Config (postPath, srcPath)
import System.Directory
  ( createDirectoryIfMissing )
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Utils.OrphanCheck"

testCases :: [TestCase]
testCases =
  [ testCheckOrphansSkipsWhenPostDirMissing
  , testCheckOrphansNoOutputWhenNoOrphans
  , testCheckOrphansWarnsWhenOrphansExist
  ]

testCheckOrphansSkipsWhenPostDirMissing :: TestCase
testCheckOrphansSkipsWhenPostDirMissing =
  mkTestCase "checkOrphans prints nothing when post directory is missing" $ do
    output <- runCheckOrphansInIsolatedWorkspace "checkOrphansSkipsWhenPostDirMissing" $ pure ()
    assertEq "missing post directory should be treated as nothing to check" "" output

testCheckOrphansNoOutputWhenNoOrphans :: TestCase
testCheckOrphansNoOutputWhenNoOrphans =
  mkTestCase "checkOrphans prints nothing when every html has matching source" $ do
    output <- runCheckOrphansInIsolatedWorkspace "checkOrphansNoOutputWhenNoOrphans" $ do
      createDirectoryIfMissing True postPath
      createDirectoryIfMissing True srcPath
      writeFile (postPath </> "a.html") "<html></html>"
      writeFile (srcPath </> "a.md") "# A"
    assertEq "no orphan outputs should produce no warning lines" "" output

testCheckOrphansWarnsWhenOrphansExist :: TestCase
testCheckOrphansWarnsWhenOrphansExist =
  mkTestCase "checkOrphans prints warning lines for orphan html outputs" $ do
    output <- runCheckOrphansInIsolatedWorkspace "checkOrphansWarnsWhenOrphansExist" $ do
      createDirectoryIfMissing True postPath
      createDirectoryIfMissing True srcPath
      writeFile (postPath </> "orphan.html") "<html>orphan</html>"
      writeFile (postPath </> "matched.html") "<html>matched</html>"
      writeFile (srcPath </> "matched.md") "# matched"
    assertContains "should print warning header before listing orphans" "[WARNING] Source file missing:" output
    assertContains "should include orphan html path in warning output" "orphan.html" output

runCheckOrphansInIsolatedWorkspace :: String -> IO () -> IO String
runCheckOrphansInIsolatedWorkspace caseName setupAction = do
  withCasePathsInSandbox suiteName caseName [] $ \casePaths -> do
    let workRoot = caseRootDir casePaths
        repoRoot = repoRootPath
        builderSourceIncludePath = repoRoot </> "Src"
        runnerPath = workRoot </> "RunOrphanCheck.hs"
    setupAction
    writeFile runnerPath runnerSource
    readProcess "runghc" ["-i" ++ builderSourceIncludePath, runnerPath] ""
  where
    runnerSource =
      unlines
        [ "import Modules.Utils.OrphanCheck (checkOrphans)"
        , "main :: IO ()"
        , "main = checkOrphans"
        ]
