module Test.Framework.Paths
  ( CasePaths(..)
  , withCasePathsInSandbox
  , srcFile
  , postFile
  , tempFile
  , templateFile
  , fixtureRoot
  , srcFixtureFile
  , templateFixtureFile
  , componentFixtureFile
  , parsePostFixturePath
  , postTemplateFixturePath
  , commonHeadFixturePath
  , navbarFixturePath
  , repoRootPath
  , withWorkDir
  , withPrependedPath
  , withEnv
  , withRedirectedStdoutToTempLog
  ) where

import Modules.Config (rootPath)
import Control.Exception (bracket, bracket_)
import System.Directory
  ( createDirectoryIfMissing
  , makeAbsolute
  , doesDirectoryExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , removePathForcibly
  , setCurrentDirectory
  )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (IOMode(AppendMode), hClose, hFlush, stdout, withFile)

-- Structured path bundle for one UT case filesystem sandbox.
data CasePaths = CasePaths
  { caseRootDir :: FilePath
  , caseSrcDir :: FilePath
  , casePostDir :: FilePath
  , caseTempDir :: FilePath
  , caseTemplateDir :: FilePath
  }

-- Creates a clean per-case workspace under Test/UT/.mock/<suite>/<case>.
--
-- `requiredDirs` is a list like ["src", "post"] or ["temp", "template"].
-- Only the requested directories are created.
withCasePaths :: String -> String -> [FilePath] -> (CasePaths -> IO a) -> IO a
withCasePaths suiteName caseName requiredDirs action = do
  exists <- doesDirectoryExist rootDir
  if exists then removePathForcibly rootDir else pure ()
  createDirectoryIfMissing True rootDir
  mapM_ (createDirectoryIfMissing True . (rootDir </>)) requiredDirs
  action paths
  where
    rootDir = utMockRoot </> suiteName </> caseName
    paths =
      CasePaths
        { caseRootDir = rootDir
        , caseSrcDir = rootDir </> "src"
        , casePostDir = rootDir </> "post"
        , caseTempDir = rootDir </> "temp"
        , caseTemplateDir = rootDir </> "template"
        }

-- Creates a clean per-case workspace and runs the action with CWD switched
-- to that case root so modules using relative config paths stay isolated.
withCasePathsInSandbox :: String -> String -> [FilePath] -> (CasePaths -> IO a) -> IO a
withCasePathsInSandbox suiteName caseName requiredDirs action =
  withCasePaths suiteName caseName requiredDirs $ \casePaths ->
    withWorkDir (caseRootDir casePaths) (action casePaths)

srcFile :: CasePaths -> FilePath -> FilePath
srcFile casePaths fileName = caseSrcDir casePaths </> fileName

postFile :: CasePaths -> FilePath -> FilePath
postFile casePaths fileName = casePostDir casePaths </> fileName

tempFile :: CasePaths -> FilePath -> FilePath
tempFile casePaths fileName = caseTempDir casePaths </> fileName

templateFile :: CasePaths -> FilePath -> FilePath
templateFile casePaths fileName = caseTemplateDir casePaths </> fileName

-- Root directory for all static UT fixtures.
fixtureRoot :: FilePath
fixtureRoot = utRoot </> ".fixture"

-- Source-markdown fixture file path under `.fixture/src`.
srcFixtureFile :: FilePath -> FilePath
srcFixtureFile fileName = fixtureRoot </> "src" </> fileName

-- Template fixture file path under `.fixture/template`.
templateFixtureFile :: FilePath -> FilePath
templateFixtureFile fileName = fixtureRoot </> "template" </> fileName

-- Component fixture file path under `.fixture/template/component`.
componentFixtureFile :: FilePath -> FilePath
componentFixtureFile fileName = fixtureRoot </> "template" </> "component" </> fileName

parsePostFixturePath :: FilePath
parsePostFixturePath = srcFixtureFile "parse-post-fixture.md"

postTemplateFixturePath :: FilePath
postTemplateFixturePath = templateFixtureFile "post.html"

commonHeadFixturePath :: FilePath
commonHeadFixturePath = componentFixtureFile "common_head.html"

navbarFixturePath :: FilePath
navbarFixturePath = componentFixtureFile "navbar.html"

utRoot :: FilePath
utRoot = repoRootAbs </> "Test" </> "UT"

utMockRoot :: FilePath
utMockRoot = utRoot </> ".mock"

repoRootAbs :: FilePath
repoRootAbs = unsafePerformIO (makeAbsolute rootPath)
{-# NOINLINE repoRootAbs #-}

repoRootPath :: FilePath
repoRootPath = repoRootAbs

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

withEnv :: String -> String -> IO a -> IO a
withEnv key value action = do
  old <- lookupEnv key
  bracket_ (setEnv key value) (restore old) action
  where
    restore (Just oldValue) = setEnv key oldValue
    restore Nothing = unsetEnv key

withRedirectedStdoutToTempLog :: FilePath -> IO a -> IO a
withRedirectedStdoutToTempLog logFileName action = do
  tempDir <- getTemporaryDirectory
  let logDir = tempDir </> "log"
  createDirectoryIfMissing True logDir
  withFile (logDir </> logFileName) AppendMode $ \logHandle ->
    bracket (hDuplicate stdout) hClose $ \originalStdout -> do
      hFlush stdout
      bracket_ (hDuplicateTo logHandle stdout) (hDuplicateTo originalStdout stdout) action
