-- | Content-hash-based staleness checks for the incremental builder,
-- backed by the configured cache directory.
--
-- Hashing happens in-process (SHA-256, via @cryptohash-sha256@). It used
-- to shell out to @b2sum@, for consistency with how the rest of the
-- builder shells out to @pandoc@/@pyftsubset@ instead of linking against
-- them -- but that analogy doesn't hold up: those two are whole programs
-- with dependency trees kiln has no business linking, while this is a
-- hash function. Measured, the subprocess was about a third of a warm
-- build (0.7ms per spawn, and more again for piping a fingerprint in),
-- and it made @coreutils@ a runtime dependency that failed at runtime
-- when absent.
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

-- | Whether `path` is unchanged since the snapshot last recorded at
-- `cachePath` by `recordFile`. Checks mtime and size first, since
-- @stat@ing a file is far cheaper than reading it: the fallback hashes
-- the whole file, and for a font source that is eleven megabytes of I/O
-- the warm build has no reason to spend. Only when mtime or size
-- disagree, or there's no prior record, does this read and hash.
--
-- The hash fallback is what makes that precheck safe to rely on: a
-- @git clone@, a @cp -r@ or a fresh checkout resets every mtime without
-- changing a byte, and without the fallback each one would re-subset
-- every font -- around 500ms for the bundled CJK font against 15ms to
-- hash it. When the hash does match despite a stale mtime/size, the
-- record is refreshed so the next check's precheck is fast again.
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
      pure (cached == digestOf fingerprint)

-- | Record `fingerprint`'s hash at `cachePath`.
recordGlobal :: FilePath -> String -> IO ()
recordGlobal cachePath fingerprint = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  writeFile cachePath (digestOf fingerprint)

-- | Copy `srcPath` to `cachePath` for later reuse, creating `cachePath`'s
-- parent directory if needed.
cacheFile :: FilePath -> FilePath -> IO ()
cacheFile cachePath srcPath = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  copyFile srcPath cachePath

-- | A fingerprint of every file under `dir </> "component"`, sorted by
-- name for a stable result. Any component a page's template references
-- via a pandoc @${ name() }@ partial can change that page's rendered
-- output without the page's own template file changing, so this belongs
-- alongside a page template's own content in a cache fingerprint.
--
-- Only the files directly in it count -- partials are referenced by a
-- flat name, so that is the whole of what a template can reach, and a
-- subdirectory someone keeps there is not a partial to read as one.
--
-- Each file contributes its name and a digest of its content, and both
-- halves are load-bearing. Concatenating the contents themselves made
-- the boundaries between files invisible: moving a block from the end of
-- @a.html@ to the start of @b.html@ left the concatenation identical, so
-- every page kept its cache and its stale output, even though the two
-- partials are spliced at different places in the template. A digest is
-- fixed-width, so nothing can bleed across a boundary by construction.
-- Naming each file catches the other edit the same blind spot hid --
-- renaming a partial without touching its content, which changes what
-- @${ component/name() }@ resolves to.
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

-- | The digest of a file already on disk.
--
-- Strict on both counts, and deliberately. Reading lazily
-- (@ByteString.Lazy.readFile@) leaves the handle open until the contents
-- are consumed, and returning the digest unforced meant nothing consumed
-- them until the whole fingerprint was hashed -- so every component file
-- stayed open across the build, and writing to one failed with
-- @resource busy@. `evaluate` forces the digest's first character, which
-- is enough to drive the hash to completion and release the contents.
hashFile :: FilePath -> IO String
hashFile path = evaluate . hexDigest . SHA256.hash =<< BS.readFile path

-- | The digest of a string, taken over its UTF-8 encoding -- the same
-- bytes the fingerprint used to be piped to @b2sum@ as, so a post's CJK
-- content hashes by what it says rather than by how a locale happened to
-- narrow it.
digestOf :: String -> String
digestOf = hexDigest . SHA256.hash . TE.encodeUtf8 . T.pack

hexDigest :: BC.ByteString -> String
hexDigest = BC.unpack . Base16.encode
