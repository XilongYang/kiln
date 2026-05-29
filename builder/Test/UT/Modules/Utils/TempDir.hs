module Test.UT.Modules.Utils.TempDir (suiteName, testCases) where

import Control.Exception (throwIO)
import Modules.Utils.TempDir
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  )
import System.FilePath ((</>))
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

-- Suite for deterministic temp directory lifecycle management.
suiteName :: String
suiteName = "Utils.TempDir"

testCases :: [TestCase]
testCases =
  [ testWithTempDirRecreatesCleanDirBeforeAction
  , testWithTempDirCleansAfterSuccess
  , testWithTempDirCleansAfterFailure
  ]

-- Confirms withTempDir removes stale contents before running the action.
testWithTempDirRecreatesCleanDirBeforeAction :: TestCase
testWithTempDirRecreatesCleanDirBeforeAction =
  mkTestCase "withTempDir recreates a clean temp directory before action" $
    withCasePaths suiteName "withTempDirRecreatesCleanDirBeforeAction" ["temp"] $ \casePaths -> do
      let tempDirPath = caseTempDir casePaths
      createDirectoryIfMissing True tempDirPath
      writeFile (tempDirPath </> "stale.txt") "old"
      withTempDir tempDirPath $ do
        dirExists <- doesDirectoryExist tempDirPath
        staleExists <- doesFileExist (tempDirPath </> "stale.txt")
        assertTrue "withTempDir should create the temp directory before running the action" dirExists
        assertFalse "withTempDir should clean stale contents before running the action" staleExists
        writeFile (tempDirPath </> "fresh.txt") "new"

-- Confirms withTempDir deletes temp directory after successful action.
testWithTempDirCleansAfterSuccess :: TestCase
testWithTempDirCleansAfterSuccess =
  mkTestCase "withTempDir cleans temp directory after successful action" $
    withCasePaths suiteName "withTempDirCleansAfterSuccess" ["temp"] $ \casePaths -> do
      let tempDirPath = caseTempDir casePaths
      withTempDir tempDirPath $ writeFile (tempDirPath </> "fresh.txt") "new"
      existsAfterSuccess <- doesDirectoryExist tempDirPath
      assertFalse "withTempDir should remove the temp directory after a successful run" existsAfterSuccess

-- Confirms withTempDir still deletes temp directory when the action throws.
testWithTempDirCleansAfterFailure :: TestCase
testWithTempDirCleansAfterFailure =
  mkTestCase "withTempDir cleans temp directory after failed action" $
    withCasePaths suiteName "withTempDirCleansAfterFailure" ["temp"] $ \casePaths -> do
      let tempDirPath = caseTempDir casePaths
      assertThrows "withTempDir should still clean up when the action throws" $
        withTempDir tempDirPath $ do
          writeFile (tempDirPath </> "boom.txt") "x"
          throwIO (userError "intentional failure")
      existsAfterFailure <- doesDirectoryExist tempDirPath
      assertFalse "withTempDir should remove the temp directory after a failed run" existsAfterFailure
