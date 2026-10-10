module Kiln.Build.FontSubset (subsetFonts) where

import Control.Monad (unless, when)
import Kiln.Build.Cache (isFileFresh, recordFile)
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (FontsConfig (..), KilnConfig (..), PageConfig (..), PostPageConfig (..), TargetConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeBaseName, (</>))
import System.Process (callProcess)

-- | Subset every font listed in `config`'s `fonts.sources` down to just
-- the characters the whole site's rendered HTML actually uses, writing
-- each to a predictable @<fonts.subset-dir>/<font file base name>.woff2@
-- path. style/fonts.css (hand-maintained, untouched by this) declares the
-- matching @font-face rules directly against those paths.
--
-- A site with no local fonts to subset states `fonts` as JSON @null@;
-- nothing is subset and the subset directory is never created.
--
-- `pyftsubset` scans every rendered page (`htmlPaths`) to determine a
-- font's required characters, so a font only needs re-subsetting when
-- its own file, or at least one of those pages, has changed since the
-- last build (tracked via `Kiln.Build.Cache`, the same way as a post's
-- source) -- or when its previous output is simply missing.
subsetFonts :: KilnConfig -> [PostEntry] -> IO ()
subsetFonts config entries = mapM_ go (targetFonts target)
  where
    target = configTarget config

    go fontsConfig
      | null (fontsSources fontsConfig) = pure ()
      | otherwise = do
          createDirectoryIfMissing True (fontsSubsetDir fontsConfig)
          pagesFresh <- and <$> mapM checkPage htmlPaths
          mapM_ (trySubset fontsConfig pagesFresh) (fontsSources fontsConfig)

    fontsCacheDir = targetCacheDir target </> "fonts"
    htmlPaths = postPaths ++ map pageOutput (targetPages target)
    postPaths = case targetPost target of
      Just ppc -> [postPageOutputDir ppc </> postSlug e ++ ".html" | e <- entries]
      Nothing  -> []

    -- | Pages are watched, not produced, by this step, so their cached
    -- snapshot is always refreshed to the page's current state --
    -- whether or not that leaves any font needing to be re-subset.
    checkPage path = do
      let cachePath = fontsCacheDir </> "pages" </> takeBaseName path ++ ".src"
      fresh <- isFileFresh cachePath path
      recordFile cachePath path
      pure fresh

    trySubset fontsConfig pagesFresh fontPath = do
      let woffPath = fontsSubsetDir fontsConfig </> (takeBaseName fontPath ++ ".woff2")
          fontCachePath = fontsCacheDir </> "files" </> takeBaseName fontPath ++ ".src"
      fontExists <- doesFileExist fontPath
      when fontExists $ do
        fontFresh <- isFileFresh fontCachePath fontPath
        recordFile fontCachePath fontPath
        woffExists <- doesFileExist woffPath
        unless (pagesFresh && fontFresh && woffExists) $
          subsetFont fontPath woffPath htmlPaths

-- | Run `pyftsubset` (from the `fonttools` package) against `fontPath`
-- (which must already exist), writing the characters covered by any of
-- `htmlPaths` to `woffPath` as WOFF2.
subsetFont :: FilePath -> FilePath -> [FilePath] -> IO ()
subsetFont fontPath woffPath htmlPaths =
  callProcess
    "pyftsubset"
    ( [ fontPath
      , "--output-file=" ++ woffPath
      , "--flavor=woff2"
      ]
        ++ ["--text-file=" ++ p | p <- htmlPaths]
    )
