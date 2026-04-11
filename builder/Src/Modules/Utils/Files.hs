module Modules.Utils.Files (writeFileWithDirectory, hashCheck, hashUpdate, hashPath) where

import Data.List (sort)
import Modules.Utils.Sha256
import System.Directory
import System.FilePath
import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad (forM)
import Modules.Config (tempPath)

-- ---[ Overview ]------------------------------------------------------------
-- | File IO and hash-state helpers for incremental build decisions.
--
-- This module provides filesystem primitives used by incremental orchestration:
-- - write file with automatic parent-directory creation
-- - compare current hash with persisted state
-- - update persisted hash state
-- - hash either a single file or an entire directory tree

-- ---[ Public API ]------------------------------------------------------------

-- | Writes file content and creates parent directory if needed.
writeFileWithDirectory :: FilePath -> String -> IO ()
writeFileWithDirectory path content = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path content

-- | Checks whether current content hash matches persisted hash state.
--
-- Returns:
-- - 'True'  when @statePath@ exists and stored hash equals current hash
-- - 'False' when hash changed or state file is missing
--
-- Hash source precedence:
-- 1) temp-state file under @tempPath/statePath@ (if present)
-- 2) recompute from @path@
hashCheck :: FilePath -> FilePath -> IO Bool
hashCheck path statePath = do
  hash <- takeHash path
  cacheFileExist <- doesFileExist statePath
  checkHash cacheFileExist hash
  where
    takeHash :: FilePath -> IO String
    takeHash path = do
      let tempStatePath = tempPath </> statePath 
      tempHashExist <- doesFileExist tempStatePath
      if tempHashExist then 
        readFileStrict tempStatePath 
      else 
        hashPath path

    checkHash :: Bool -> String -> IO Bool
    checkHash fileExist hash
      | fileExist = do
        lastHash <- readFileStrict statePath 
        if (lastHash /= hash) then do 
          return False
        else do
          return True
      | otherwise = do
        writeFileWithDirectory statePath hash
        return False

-- | Recomputes hash for @path@ and updates persisted @statePath@.
hashUpdate :: FilePath -> FilePath -> IO ()
hashUpdate path statePath = do
  hash <- hashPath path
  writeFileWithDirectory statePath hash

-- | Hashes a file path or directory path.
--
-- - file: SHA256 of file content
-- - dir : deterministic SHA256 over all recursive child files
--
-- Throws when path does not exist.
hashPath :: FilePath -> IO String
hashPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  hashPath' isDir isFile
  where
    hashPath' True _ = hashDir path
    hashPath' _ True = hashFile path
    hashPath' _ _ = error ("hashPath: path not exist:" ++ path)

-- ---[ Implementation Details ]-----------------------------------------------

-- | Strict file read helper.
--
-- Forces the full file content in memory before returning, so caller logic
-- does not accidentally depend on lazy IO timing.
readFileStrict :: FilePath -> IO String
readFileStrict path =
  withFile path ReadMode $ \h -> do
    s <- hGetContents h
    evaluate (force s)
    pure s

-- | Hashes one file by its full content.
hashFile :: FilePath -> IO String
hashFile path = do
  fileContent <- readFile path
  return (sha256Hex fileContent)

-- | Hashes a directory deterministically.
--
-- Recursively collects all file paths, sorts them for stable ordering, hashes
-- each file, then hashes the concatenated per-file hashes.
hashDir :: FilePath -> IO String
hashDir path = do
  srcCodePaths <- sort <$> listFilesRecursive path
  srcHashs <- mapM hashFile srcCodePaths 
  return (finalHash srcHashs)
  where
    finalHash = (sha256Hex . concat)

-- | Lists all files under directory recursively.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive path = do
    contents <- listDirectory path
    paths <- forM contents $ \name -> do
        let fullPath = path </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listFilesRecursive fullPath
            else return [fullPath]
    return (concat paths)
