module UT.Modules.TempDir (suiteName, testCases) where

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

tempDirPath :: FilePath
tempDirPath = "builder/UT/.mock/tempdir"

-- Suite for deterministic temp directory lifecycle management.
suiteName :: String
suiteName = "TempDir"

testCases :: [TestCase]
testCases =
  [ -- Test Case#1: Existing stale files should be removed before the action starts.
    mkTestCase "withTempDir recreates a clean temp directory before action" $ do
      createDirectoryIfMissing True tempDirPath
      writeFile (tempDirPath </> "stale.txt") "old"

      withTempDir tempDirPath $ do
        dirExists <- doesDirectoryExist tempDirPath
        staleExists <- doesFileExist (tempDirPath </> "stale.txt")
        assertTrue "withTempDir should create the temp directory before running the action" dirExists
        assertFalse "withTempDir should clean stale contents before running the action" staleExists
        writeFile (tempDirPath </> "fresh.txt") "new"
  , -- Test Case#2: Successful action completion should still remove the temp directory.
    mkTestCase "withTempDir cleans temp directory after successful action" $ do
      withTempDir tempDirPath $ writeFile (tempDirPath </> "fresh.txt") "new"
      existsAfterSuccess <- doesDirectoryExist tempDirPath
      assertFalse "withTempDir should remove the temp directory after a successful run" existsAfterSuccess
  , -- Test Case#3: Cleanup must also happen when the action throws an exception.
    mkTestCase "withTempDir cleans temp directory after failed action" $ do
      assertThrows "withTempDir should still clean up when the action throws" $
        withTempDir tempDirPath $ do
          writeFile (tempDirPath </> "boom.txt") "x"
          throwIO (userError "intentional failure")

      existsAfterFailure <- doesDirectoryExist tempDirPath
      assertFalse "withTempDir should remove the temp directory after a failed run" existsAfterFailure
  ]
