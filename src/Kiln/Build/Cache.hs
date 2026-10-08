-- | Content-hash-based staleness checks for the incremental builder,
-- backed by @.cache@. Hashing is delegated to the external @b2sum@
-- (BLAKE2b, from coreutils) rather than a Haskell hashing library, to
-- stay consistent with how the rest of the builder already shells out to
-- @pandoc@/@pyftsubset@ instead of linking against their libraries.
module Kiln.Build.Cache
  ( isFileFresh
  , recordFile
  , isGlobalFresh
  , recordGlobal
  , cacheFile
  ) where

import Control.Monad (when)
import Data.Time.Clock (UTCTime)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime)
import System.FilePath (takeDirectory)
import System.IO (readFile')
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Whether `path` is unchanged since the snapshot last recorded at
-- `cachePath` by `recordFile`. Checks mtime and size first, since
-- @stat@ing a file is far cheaper than hashing it (spawning @b2sum@
-- costs low-single-digit milliseconds regardless of file size, dwarfing
-- the actual hashing work for a file this size); only when they
-- disagree, or there's no prior record, does this fall back to
-- comparing content hashes. When the hash still matches despite a stale
-- mtime/size (the file was touched but not actually edited), the record
-- is refreshed so the next check's mtime/size precheck is fast again.
isFileFresh :: FilePath -> FilePath -> IO Bool
isFileFresh cachePath path = do
  prior <- readSnapshot cachePath
  case prior of
    Nothing -> pure False
    Just (mtime, size, hash) -> do
      mtime' <- getModificationTime path
      size' <- getFileSize path
      if mtime' == mtime && size' == size
        then pure True
        else do
          hash' <- hashFile path
          let unchanged = hash' == hash
          when unchanged $ writeSnapshot cachePath (mtime', size', hash)
          pure unchanged

-- | Record `path`'s current mtime, size, and content hash at
-- `cachePath`, for a later `isFileFresh` to compare against.
recordFile :: FilePath -> FilePath -> IO ()
recordFile cachePath path = do
  mtime <- getModificationTime path
  size <- getFileSize path
  hash <- hashFile path
  writeSnapshot cachePath (mtime, size, hash)

-- | Whether `fingerprint` matches the one last recorded at `cachePath` by
-- `recordGlobal`.
isGlobalFresh :: FilePath -> String -> IO Bool
isGlobalFresh cachePath fingerprint = do
  exists <- doesFileExist cachePath
  if not exists
    then pure False
    else do
      cached <- readFile' cachePath
      current <- hashString fingerprint
      pure (cached == current)

-- | Record `fingerprint`'s hash at `cachePath`.
recordGlobal :: FilePath -> String -> IO ()
recordGlobal cachePath fingerprint = do
  hash <- hashString fingerprint
  createDirectoryIfMissing True (takeDirectory cachePath)
  writeFile cachePath hash

-- | Copy `srcPath` to `cachePath` for later reuse, creating `cachePath`'s
-- parent directory if needed.
cacheFile :: FilePath -> FilePath -> IO ()
cacheFile cachePath srcPath = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  copyFile srcPath cachePath

readSnapshot :: FilePath -> IO (Maybe (UTCTime, Integer, String))
readSnapshot path = do
  exists <- doesFileExist path
  if not exists then pure Nothing else readMaybe <$> readFile' path

writeSnapshot :: FilePath -> (UTCTime, Integer, String) -> IO ()
writeSnapshot path snapshot = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (show snapshot)

-- | The b2sum digest of a file already on disk.
hashFile :: FilePath -> IO String
hashFile path = hashOutput <$> readProcess "b2sum" [path] ""

-- | The b2sum digest of a string, piped in over stdin.
hashString :: String -> IO String
hashString s = hashOutput <$> readProcess "b2sum" [] s

hashOutput :: String -> String
hashOutput = takeWhile (/= ' ')
