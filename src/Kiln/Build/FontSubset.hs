module Kiln.Build.FontSubset (subsetFonts) where

import Control.Monad (unless, when)
import qualified Data.Set as Set
import Kiln.Build.Cache (isFileFresh, isGlobalFresh, recordFile, recordGlobal)
import Kiln.Build.Env (BuildEnv (..), cachePath, logMsg, mapBounded, postOutputPath, withContext, workPath)
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
-- A font is re-subset only when its own file changed, when its output is
-- missing, or when the site's rendered pages brought a character the
-- last subset didn't cover -- rewording a post leaves every glyph it
-- needs already in there.
subsetFonts :: BuildEnv -> [PostEntry] -> IO ()
subsetFonts env entries = mapM_ go (envFonts env)
  where
    go fontsConfig
      | null (fontsSources fontsConfig) = pure ()
      | otherwise = do
          createDirectoryIfMissing True (fontsSubsetDir fontsConfig)
          charset <- siteCharset
          textFile <- writeSubsetText charset
          let fingerprint = unlines [envTools env, charset]
          charsFresh <- isGlobalFresh charsetCachePath fingerprint
          _ <- mapBounded (trySubset fontsConfig charsFresh textFile) (fontsSources fontsConfig)
          -- Recorded only once every font is written: a subset that died
          -- halfway must not leave the next build calling it fresh.
          recordGlobal charsetCachePath fingerprint

    htmlPaths = postPaths ++ map pageOutput (envPages env)
    postPaths = [p | e <- entries, Just p <- [postOutputPath env (postSlug e)]]
    charsetCachePath = cachePath env ["fonts", "charset"]

    -- | Every character the rendered site uses, once each and in order.
    -- A set, not the pages themselves: it is what a subset is made of,
    -- and what decides whether an existing one still covers the site.
    siteCharset = Set.toAscList . Set.unions <$> mapM charsOf htmlPaths
    charsOf = fmap Set.fromList . readIfExists

    -- | The characters in one file for pyftsubset's @--text-file@: a
    -- flag per character would overrun the command-line length limit.
    writeSubsetText charset = do
      let path = workPath env ["subset-text.txt"]
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path charset
      pure path

    trySubset fontsConfig charsFresh textFile fontPath =
      withContext ("while subsetting " ++ fontPath) $ do
        let woffPath = fontsSubsetDir fontsConfig </> (takeBaseName fontPath ++ ".woff2")
            fontCachePath = cachePath env ["fonts", "files", fontPath ++ ".src"]
        fontExists <- doesFileExist fontPath
        when fontExists $ do
          fontFresh <- isFileFresh fontCachePath fontPath
          unless fontFresh (recordFile fontCachePath fontPath)
          woffExists <- doesFileExist woffPath
          unless (charsFresh && fontFresh && woffExists) $ do
            logMsg ("  " ++ fontPath)
            subsetFont fontPath woffPath textFile

readIfExists :: FilePath -> IO String
readIfExists path = do
  exists <- doesFileExist path
  if exists then readFile' path else pure ""

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
