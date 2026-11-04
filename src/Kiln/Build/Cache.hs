-- | Content-hash staleness checks for the incremental builder, backed by
-- the configured cache directory.
--
-- Hashed in-process (SHA-256): a checksum subprocess per file cost about
-- a third of a warm build, and made @coreutils@ a runtime dependency.
module Kiln.Build.Cache
  ( isFileFresh
  , recordFile
  , isGlobalFresh
  , recordGlobal
  , cacheFile
  , componentsFingerprint
  ) where

import Control.Exception (evaluate)
import Control.Monad (filterM, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , listDirectory
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import Text.Read (readMaybe)

-- | Whether `path` is unchanged since `recordFile` recorded it at
-- `cachePath`. mtime and size are checked first because hashing a font
-- source is megabytes of I/O a warm build needn't spend; the hash
-- fallback is what makes that safe, since a clone or a copy resets mtime
-- without changing a byte.
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
          -- Stale mtime over identical content: refresh so the next
          -- check stops at the precheck again.
          when unchanged $ writeSnapshot cachePath (mtime', size', hash)
          pure unchanged

-- | Record `path`'s mtime, size and content hash for `isFileFresh`.
recordFile :: FilePath -> FilePath -> IO ()
recordFile cachePath path = do
  mtime <- getModificationTime path
  size <- getFileSize path
  hash <- hashFile path
  writeSnapshot cachePath (mtime, size, hash)

-- | Whether `fingerprint` matches the one `recordGlobal` recorded.
isGlobalFresh :: FilePath -> String -> IO Bool
isGlobalFresh cachePath fingerprint = do
  exists <- doesFileExist cachePath
  if not exists
    then pure False
    else do
      cached <- readFile' cachePath
      pure (cached == digestOf fingerprint)

-- | Record `fingerprint`'s hash for `isGlobalFresh`.
recordGlobal :: FilePath -> String -> IO ()
recordGlobal cachePath fingerprint = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  writeFile cachePath (digestOf fingerprint)

-- | Copy `srcPath` to `cachePath` for later reuse.
cacheFile :: FilePath -> FilePath -> IO ()
cacheFile cachePath srcPath = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  copyFile srcPath cachePath

-- | A fingerprint of the files directly under @dir\/component@, sorted
-- for stability. A page's cache has to cover these: a template splices
-- them in as pandoc partials, so one can change the rendered page
-- without the template file changing. Only that one level, since a
-- partial is referenced by a flat name.
--
-- Name plus digest per file: a fixed-width digest keeps content from
-- bleeding across a file boundary (a block moved between two partials),
-- and the name catches a rename that changes what a partial resolves
-- to.
componentsFingerprint :: FilePath -> IO String
componentsFingerprint dir = do
  let compDir = dir </> "component"
  exists <- doesDirectoryExist compDir
  if not exists
    then pure ""
    else do
      names <- sort <$> listDirectory compDir
      files <- filterM (doesFileExist . (compDir </>)) names
      concat <$> mapM (describeFile compDir) files
  where
    describeFile compDir name = do
      digest <- hashFile (compDir </> name)
      pure (name ++ " " ++ digest ++ "\n")

readSnapshot :: FilePath -> IO (Maybe (UTCTime, Integer, String))
readSnapshot path = do
  exists <- doesFileExist path
  if not exists then pure Nothing else readMaybe <$> readFile' path

writeSnapshot :: FilePath -> (UTCTime, Integer, String) -> IO ()
writeSnapshot path snapshot = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (show snapshot)

-- | Strict on both counts: a lazy read holds the handle open until the
-- digest is consumed, which keeps every file open across the build and
-- fails a write to one with @resource busy@.
hashFile :: FilePath -> IO String
hashFile path = evaluate . hexDigest . SHA256.hash =<< BS.readFile path

-- | Digest of a string over its UTF-8 encoding, so CJK content hashes
-- by what it says rather than by the locale.
digestOf :: String -> String
digestOf = hexDigest . SHA256.hash . TE.encodeUtf8 . T.pack

hexDigest :: BC.ByteString -> String
hexDigest = BC.unpack . Base16.encode
