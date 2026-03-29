module Test.UT.Modules.Utils.OrphanCheck (suiteName, testCases) where

import Control.Exception (bracket_)
import Modules.Config (postPath, srcPath)
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , getTemporaryDirectory
  , setCurrentDirectory
  )
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Framework.Asserts
import Test.Framework.TestSuite
import Modules.Utils.TempDir (withTempDir)

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
  repoRoot <- getCurrentDirectory
  tempRoot <- getTemporaryDirectory
  let workRoot = tempRoot </> ("xilong-site-ut-orphan-" ++ caseName)
      builderSourceIncludePath = repoRoot </> "builder" </> "Src"
      runnerPath = workRoot </> "RunOrphanCheck.hs"
  withTempDir workRoot $ do
    withWorkDir workRoot setupAction
    writeFile runnerPath runnerSource
    withWorkDir workRoot $
      readProcess "runghc" ["-i" ++ builderSourceIncludePath, runnerPath] ""
  where
    runnerSource =
      unlines
        [ "import Modules.Utils.OrphanCheck (checkOrphans)"
        , "main :: IO ()"
        , "main = checkOrphans"
        ]

withWorkDir :: FilePath -> IO a -> IO a
withWorkDir dir action = do
  old <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory old) action
