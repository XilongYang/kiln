module UT.TestUtils.Paths
  ( CasePaths(..)
  , withCasePaths
  , srcFile
  , postFile
  , tempFile
  , templateFile
  ) where

import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , removePathForcibly
  )
import System.FilePath ((</>))

-- Structured path bundle for one UT case filesystem sandbox.
data CasePaths = CasePaths
  { caseRootDir :: FilePath
  , caseSrcDir :: FilePath
  , casePostDir :: FilePath
  , caseTempDir :: FilePath
  , caseTemplateDir :: FilePath
  }

-- Creates a clean per-case workspace under builder/UT/.mock/<suite>/<case>.
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
    rootDir = "builder/UT/.mock" </> suiteName </> caseName
    paths =
      CasePaths
        { caseRootDir = rootDir
        , caseSrcDir = rootDir </> "src"
        , casePostDir = rootDir </> "post"
        , caseTempDir = rootDir </> "temp"
        , caseTemplateDir = rootDir </> "template"
        }

srcFile :: CasePaths -> FilePath -> FilePath
srcFile casePaths fileName = caseSrcDir casePaths </> fileName

postFile :: CasePaths -> FilePath -> FilePath
postFile casePaths fileName = casePostDir casePaths </> fileName

tempFile :: CasePaths -> FilePath -> FilePath
tempFile casePaths fileName = caseTempDir casePaths </> fileName

templateFile :: CasePaths -> FilePath -> FilePath
templateFile casePaths fileName = caseTemplateDir casePaths </> fileName
