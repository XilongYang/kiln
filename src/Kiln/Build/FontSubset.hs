module Kiln.Build.FontSubset (subsetFonts) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..))
import Paths_kiln (getDataDir)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.IO.Error (ioError, userError)
import System.Process (readProcessWithExitCode)

-- | For every post, and once for the whole site, subset every font
-- declared in `config`'s `fonts` map down to just the characters its
-- rendered HTML actually uses and write a CSS file declaring
-- `@font-face` rules (with accurate `unicode-range`) for the fonts that
-- were used, each under its own tier-specific name (see `subsetFontsFor`).
subsetFonts :: KilnConfig -> [PostEntry] -> IO ()
subsetFonts config entries = do
  createDirectoryIfMissing True subsetDir
  dataDir <- getDataDir
  let scriptPath = dataDir </> "tools" </> "subset.py"
  mapM_ (\e -> subsetFontsFor scriptPath fontsDir localFonts [postSlug e, "Site"] (subsetDir </> postSlug e) [postPath e]) entries
  subsetFontsFor scriptPath fontsDir localFonts ["Site"] (subsetDir </> "site") (map postPath entries ++ [indexPath])
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    fontsDir = inFonts inPaths
    postDir = outPost outPaths
    indexPath = outIndex outPaths
    subsetDir = outFontsSubset outPaths
    localFonts = Map.toList (configFonts config)
    postPath e = postDir </> postSlug e ++ ".html"

-- | Subset every font in `localFonts` against `htmlPaths`, writing
-- `<prefix>.css` declaring the fonts that were actually used (each
-- pointing at its own `<prefix>-<font>.woff2`), under `'<family>
-- (<tierLabels head>)'` plus a `--font-stack-<font file>` custom property
-- chaining the rest of `tierLabels` and the plain `family` as fallbacks.
subsetFontsFor :: FilePath -> FilePath -> [(FilePath, String)] -> [String] -> FilePath -> [FilePath] -> IO ()
subsetFontsFor scriptPath fontsDir localFonts tierLabels prefix htmlPaths = do
  rules <- mapM trySubset localFonts
  writeFile (prefix ++ ".css") (concat [rule | Just rule <- rules])
  where
    trySubset (fileName, family) = do
      let tierNames = [family ++ " (" ++ label ++ ")" | label <- tierLabels] ++ [family]
          tieredFamily = case tierLabels of
            (label : _) -> family ++ " (" ++ label ++ ")"
            [] -> family
          fontPath = fontsDir </> fileName
          woffPath = prefix ++ "-" ++ takeBaseName fileName ++ ".woff2"
      exists <- doesFileExist fontPath
      if exists
        then do
          result <- subsetFont scriptPath fontPath woffPath htmlPaths
          pure (fontFaceAndStackRule fileName tieredFamily tierNames woffPath <$> result)
        else pure Nothing

-- | Run `tools/subset.py` against `fontPath` (which must already exist),
-- writing the subset to `woffPath` if any of `htmlPaths`'s characters are
-- covered by this font. Returns the resulting CSS `unicode-range` value,
-- or `Nothing` if the font genuinely isn't used by any of `htmlPaths`
-- (exit code 2, see `tools/subset.py`). Any other failure (corrupt font,
-- etc.) aborts the build instead of silently being treated as "unused".
subsetFont :: FilePath -> FilePath -> FilePath -> [FilePath] -> IO (Maybe String)
subsetFont scriptPath fontPath woffPath htmlPaths = do
  (code, out, err) <- readProcessWithExitCode "python3" (scriptPath : fontPath : woffPath : htmlPaths) ""
  case (code, lines out) of
    (ExitSuccess, unicodeRange : _) -> pure (Just unicodeRange)
    (ExitFailure 2, _) -> pure Nothing
    _ ->
      ioError . userError $
        "failed to subset " ++ fontPath ++ " for " ++ show htmlPaths ++ ": " ++ err

fontFaceAndStackRule :: FilePath -> String -> [String] -> FilePath -> String -> String
fontFaceAndStackRule fileName tieredFamily tierNames woffPath unicodeRange =
  fontFaceRule tieredFamily woffPath unicodeRange ++ stackRule fileName tierNames

fontFaceRule :: String -> FilePath -> String -> String
fontFaceRule family woffPath unicodeRange =
  "@font-face { font-family: '"
    ++ family
    ++ "'; src: url('"
    ++ takeBaseName woffPath
    ++ ".woff2') format('woff2'); unicode-range: "
    ++ unicodeRange
    ++ "; }\n"

stackRule :: FilePath -> [String] -> String
stackRule fileName tierNames =
  ":root { --font-stack-"
    ++ takeBaseName fileName
    ++ ": "
    ++ intercalate ", " [ "'" ++ name ++ "'" | name <- tierNames ]
    ++ "; }\n"
