module UT.Modules.TempDir (suiteName, testCases) where

import Modules.Config
import Modules.Utils.TempDir
import UT.TestUtils.TestSuite
import UT.TestUtils.Asserts

import Control.Exception (throwIO)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  )
import System.FilePath ((</>))
import System.IO (writeFile)

-- Suite for deterministic temp directory lifecycle management.
suiteName :: String
suiteName = "TempDir"

testCases :: [TestCase]
testCases =
  [ -- Test Case#1: Existing stale files should be removed before the action starts.
    mkTestCase "withTempDir recreates a clean temp directory before action" $ do
      createDirectoryIfMissing True tempPath
      writeFile (tempPath </> "stale.txt") "old"

      withTempDir $ do
        dirExists <- doesDirectoryExist tempPath
        staleExists <- doesFileExist (tempPath </> "stale.txt")
        assertTrue "withTempDir should create the temp directory before running the action" dirExists
        assertFalse "withTempDir should clean stale contents before running the action" staleExists
        writeFile (tempPath </> "fresh.txt") "new"
  , -- Test Case#2: Successful action completion should still remove the temp directory.
    mkTestCase "withTempDir cleans temp directory after successful action" $ do
      withTempDir $ writeFile (tempPath </> "fresh.txt") "new"
      existsAfterSuccess <- doesDirectoryExist tempPath
      assertFalse "withTempDir should remove the temp directory after a successful run" existsAfterSuccess
  , -- Test Case#3: Cleanup must also happen when the action throws an exception.
    mkTestCase "withTempDir cleans temp directory after failed action" $ do
      assertThrows "withTempDir should still clean up when the action throws" $
        withTempDir $ do
          writeFile (tempPath </> "boom.txt") "x"
          throwIO (userError "intentional failure")

      existsAfterFailure <- doesDirectoryExist tempPath
      assertFalse "withTempDir should remove the temp directory after a failed run" existsAfterFailure
  ]
