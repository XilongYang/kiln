module Kiln.Build.FontSubset (subsetFonts) where

import Control.Monad (unless, when)
import Data.List (sort)
import Kiln.Build.Cache (isFileFresh, recordFile)
import Kiln.Build.Env (BuildEnv (..), cachePath, logMsg, postOutputPath, warn, withContext, workPath)
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (FontsConfig (..), PageConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO (readFile')
import System.Process (callProcess)

-- | Subset every font listed in `env`'s `fonts.sources` down to just
-- the characters the whole site's rendered HTML actually uses, writing
-- each to a predictable @<fonts.subset-dir>/<font file base name>.woff2@
-- path. style/fonts.css (hand-maintained, untouched by this) declares the
-- matching @font-face rules directly against those paths.
--
-- A site with no local fonts to subset states `fonts` as JSON @null@;
-- nothing is subset and the subset directory is never created.
--
-- `pyftsubset` scans every rendered page to determine a font's required
-- characters, so a font only needs re-subsetting when its own file, or at
-- least one of those pages, has changed since the last build (tracked via
-- `Kiln.Build.Cache`, the same way as a post's source) -- or when its
-- previous output is simply missing.
subsetFonts :: BuildEnv -> [PostEntry] -> IO ()
subsetFonts env entries = mapM_ go (envFonts env)
  where
    go fontsConfig
      | null (fontsSources fontsConfig) = pure ()
      | otherwise = do
          warnOnSharedBaseNames (fontsSources fontsConfig)
          createDirectoryIfMissing True (fontsSubsetDir fontsConfig)
          pagesFresh <- and <$> mapM checkPage htmlPaths
          textFile <- writeSubsetText
          mapM_ (trySubset fontsConfig pagesFresh textFile) (fontsSources fontsConfig)

    htmlPaths = postPaths ++ map pageOutput (envPages env)
    postPaths = [p | e <- entries, Just p <- [postOutputPath env (postSlug e)]]

    -- | Pages are watched, not produced, by this step, so a changed page
    -- has its snapshot refreshed here -- whether or not that leaves any
    -- font needing to be re-subset.
    --
    -- Only a changed one, though: `isFileFresh` answering `True` already
    -- means the snapshot matches the file, so re-recording it would buy
    -- nothing and cost a full hash of the file. Recording regardless was
    -- re-hashing every rendered page and every font source on every
    -- build -- around 11 MB on the bundled @default@ template, almost
    -- all of it one CJK font -- and throwing away exactly the work the
    -- mtime/size precheck exists to avoid.
    --
    -- The cache entry mirrors the page's own output path, the way
    -- `Kiln.Build.Page` does: keyed on the base name alone, two pages
    -- whose outputs merely share a name ("post/a.html" and
    -- "about/a.html") would have shared one entry and masked each
    -- other's changes.
    checkPage path = do
      let cache = cachePath env ["fonts", "pages", path ++ ".src"]
      fresh <- isFileFresh cache path
      unless fresh (recordFile cache path)
      pure fresh

    -- | Concatenate every rendered page into one file for pyftsubset's
    -- @--text-file@. One flag rather than one per page: the flags are a
    -- command line, and a site of a few thousand posts would overrun the
    -- system's limit on how long one can be.
    writeSubsetText = do
      let path = workPath env ["subset-text.txt"]
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path . concat =<< mapM readIfExists htmlPaths
      pure path

    trySubset fontsConfig pagesFresh textFile fontPath =
      withContext ("while subsetting " ++ fontPath) $ do
        let woffPath = fontsSubsetDir fontsConfig </> (takeBaseName fontPath ++ ".woff2")
            fontCachePath = cachePath env ["fonts", "files", fontPath ++ ".src"]
        fontExists <- doesFileExist fontPath
        when fontExists $ do
          fontFresh <- isFileFresh fontCachePath fontPath
          unless fontFresh (recordFile fontCachePath fontPath)
          woffExists <- doesFileExist woffPath
          unless (pagesFresh && fontFresh && woffExists) $ do
            logMsg ("  " ++ fontPath)
            subsetFont fontPath woffPath textFile

readIfExists :: FilePath -> IO String
readIfExists path = do
  exists <- doesFileExist path
  if exists then readFile' path else pure ""

-- | Warn when two configured font sources share a base name. The subset
-- output is named after it, so the second would silently overwrite the
-- first -- and since @fonts.css@ names that output by hand, renaming it
-- here would break the stylesheet instead. Saying so is the useful part.
warnOnSharedBaseNames :: [FilePath] -> IO ()
warnOnSharedBaseNames sources =
  mapM_ complain (duplicates (sort (map takeBaseName sources)))
  where
    duplicates (a : b : rest)
      | a == b    = a : duplicates (dropWhile (== a) (b : rest))
      | otherwise = duplicates (b : rest)
    duplicates _ = []
    complain name =
      warn
        ( "more than one font source is named "
            ++ name
            ++ "; they all subset to "
            ++ name
            ++ ".woff2 and only the last one survives"
        )

-- | Run `pyftsubset` (from the `fonttools` package) against `fontPath`
-- (which must already exist), writing the characters covered by
-- `textFile` to `woffPath` as WOFF2.
subsetFont :: FilePath -> FilePath -> FilePath -> IO ()
subsetFont fontPath woffPath textFile =
  callProcess
    "pyftsubset"
    [ fontPath
    , "--output-file=" ++ woffPath
    , "--flavor=woff2"
    , "--text-file=" ++ textFile
    ]
