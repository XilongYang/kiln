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

-- | Subset every font in `fonts.sources` down to the characters the
-- site's rendered HTML uses, each to
-- @<fonts.subset-dir>\/<base name>.woff2@ -- a predictable path, since
-- the hand-maintained style/fonts.css names it in its @\@font-face@
-- rules. Nothing happens for a site whose `fonts` is JSON @null@.
--
-- The characters come from the rendered pages, so a font is re-subset
-- only when its own file or one of those pages changed, or when its
-- output is missing.
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

    -- | Pages are watched here, not produced, so a changed one has its
    -- snapshot refreshed. Only a changed one: a fresh answer already
    -- means the snapshot matches, and re-recording would re-hash every
    -- page and font on every build.
    --
    -- The entry mirrors the page's output path, so two outputs sharing a
    -- base name ("post/a.html", "about/a.html") can't mask each other's
    -- changes.
    checkPage path = do
      let cache = cachePath env ["fonts", "pages", path ++ ".src"]
      fresh <- isFileFresh cache path
      unless fresh (recordFile cache path)
      pure fresh

    -- | Every rendered page in one file for pyftsubset's @--text-file@:
    -- a flag per page would overrun the command-line length limit on a
    -- site of a few thousand posts.
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

-- | Warn when two font sources share a base name: the outputs are named
-- after it, so one overwrites the other. Renaming instead would break
-- the @fonts.css@ that names it by hand.
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

-- | Run @pyftsubset@ (from @fonttools@) over `fontPath`, writing the
-- characters `textFile` covers to `woffPath` as WOFF2.
subsetFont :: FilePath -> FilePath -> FilePath -> IO ()
subsetFont fontPath woffPath textFile =
  callProcess
    "pyftsubset"
    [ fontPath
    , "--output-file=" ++ woffPath
    , "--flavor=woff2"
    , "--text-file=" ++ textFile
    ]
