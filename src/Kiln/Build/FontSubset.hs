module Kiln.Build.FontSubset (subsetFonts) where

import qualified Data.Map.Strict as Map
import Kiln.Build.PostEntry (PostEntry (..))
import Kiln.Config (InPaths (..), KilnConfig (..), OutPaths (..), PathConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeBaseName, (</>))
import System.Process (callProcess)

-- | Subset every font declared in `config`'s `fonts` map down to just the
-- characters the whole site's rendered HTML actually uses, writing each
-- to a predictable @fonts-subset/<font file base name>.woff2@ path.
-- style/fonts.css (hand-maintained, untouched by this) declares the
-- matching @font-face rules directly against those paths.
subsetFonts :: KilnConfig -> [PostEntry] -> IO ()
subsetFonts config entries = do
  createDirectoryIfMissing True subsetDir
  mapM_ trySubset localFonts
  where
    inPaths = pathIn (configPath config)
    outPaths = pathOut (configPath config)
    fontsDir = inFonts inPaths
    postDir = outPost outPaths
    indexPath = outIndex outPaths
    subsetDir = outFontsSubset outPaths
    localFonts = Map.keys (configFonts config)
    htmlPaths = map postPath entries ++ [indexPath]
    postPath e = postDir </> postSlug e ++ ".html"
    trySubset fileName = do
      let fontPath = fontsDir </> fileName
          woffPath = subsetDir </> (takeBaseName fileName ++ ".woff2")
      exists <- doesFileExist fontPath
      if exists
        then subsetFont fontPath woffPath htmlPaths
        else pure ()

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
