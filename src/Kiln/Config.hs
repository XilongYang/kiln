{-# LANGUAGE OverloadedStrings #-}

module Kiln.Config
  ( KilnConfig (..)
  , InputConfig (..)
  , OptConfig (..)
  , TargetConfig (..)
  , FontsConfig (..)
  , TocConfig (..)
  , PageConfig (..)
  , PostPageConfig (..)
  , readConfig
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?), (.!=))
import System.Directory (doesFileExist)
import System.Exit (die)

-- | The read-only directories kiln finds its own source material in --
-- never written to, never cleaned.
data InputConfig = InputConfig
  { inputSrcDir      :: FilePath
  , inputTemplateDir :: FilePath
  } deriving (Show, Eq)

-- | Site-wide behavior, as opposed to filesystem locations.
data OptConfig = OptConfig
  { optWebroot :: FilePath
  , optToc     :: TocConfig
  } deriving (Show, Eq)

-- | Where to write subset fonts (`fontsSubsetDir`), and the font files
-- (full paths) to subset into it. A site with no local fonts to subset
-- states this as JSON @null@ instead, same as `targetPost` -- see
-- `Kiln.Build.FontSubset.subsetFonts`.
data FontsConfig = FontsConfig
  { fontsSubsetDir :: FilePath
  , fontsSources   :: [FilePath]
  } deriving (Show, Eq)

-- | One page to generate: its own template (a path under
-- `inputTemplateDir`), and the site-relative path to write it to. A
-- page's `pageOutput` doubles as its own identity -- two pages can't
-- share one without one silently overwriting the other's output
-- regardless of caching, so it's already a safe, unique key for the
-- page's own build cache; there's no separate name to keep in sync.
data PageConfig = PageConfig
  { pageTemplate :: FilePath
  , pageOutput   :: FilePath
  } deriving (Show, Eq)

-- | The per-post standalone page @kiln build@ renders for every
-- markdown file in `inputSrcDir`, if any -- a site like a flat
-- card-flow feed may not want one at all (see `Kiln.Config.readConfig`'s
-- caller, which requires this to be stated explicitly as either this or
-- JSON @null@, never silently defaulted). `postPageOutputDir` is a
-- directory; a post with slug @s@ is written to
-- @postPageOutputDir </> s <> ".html"@.
data PostPageConfig = PostPageConfig
  { postPageTemplate  :: FilePath
  , postPageOutputDir :: FilePath
  } deriving (Show, Eq)

data TocConfig = TocConfig
  { tocEnable :: Bool
  , tocDepth  :: Int
  } deriving (Show, Eq)

-- | Everything kiln generates and `kiln clean` removes.
data TargetConfig = TargetConfig
  { targetSearchDb :: Maybe FilePath
    -- ^ Output path for the generated search index JSON, or JSON @null@
    -- if the site has no search feature to feed -- same convention as
    -- `targetPost`/`targetFonts`. See `Kiln.Build.SearchDb.writeSearchDb`.
  , targetCacheDir :: FilePath
  , targetPost     :: Maybe PostPageConfig
  , targetPages    :: [PageConfig]
  , targetFonts    :: Maybe FontsConfig
  } deriving (Show, Eq)

data KilnConfig = KilnConfig
  { configInput  :: InputConfig
  , configOpt    :: OptConfig
  , configTarget :: TargetConfig
  } deriving (Show, Eq)

instance FromJSON InputConfig where
  parseJSON = withObject "input" $ \o ->
    InputConfig
      <$> o .: "src-dir"
      <*> o .: "template-dir"

instance FromJSON OptConfig where
  parseJSON = withObject "opt" $ \o ->
    OptConfig
      <$> o .: "webroot"
      <*> o .:? "toc" .!= TocConfig True 3

instance FromJSON FontsConfig where
  parseJSON = withObject "fonts" $ \o ->
    FontsConfig
      <$> o .: "subset-dir"
      <*> o .: "sources"

instance FromJSON PageConfig where
  parseJSON = withObject "page" $ \o ->
    PageConfig
      <$> o .: "template"
      <*> o .: "output"

instance FromJSON PostPageConfig where
  parseJSON = withObject "post" $ \o ->
    PostPageConfig
      <$> o .: "template"
      <*> o .: "output-dir"

instance FromJSON TocConfig where
  parseJSON = withObject "toc" $ \o ->
    TocConfig
      <$> o .:? "enable" .!= True
      <*> o .:? "depth" .!= 3

instance FromJSON TargetConfig where
  parseJSON = withObject "target" $ \o ->
    TargetConfig
      <$> o .: "searchdb"
      <*> o .: "cache-dir"
      <*> o .: "post"
      <*> o .: "pages"
      <*> o .: "fonts"

instance FromJSON KilnConfig where
  parseJSON = withObject "kiln-config" $ \o ->
    KilnConfig
      <$> o .: "input"
      <*> o .: "opt"
      <*> o .: "target"

configFileName :: FilePath
configFileName = "kiln-config.json"

readConfig :: IO KilnConfig
readConfig = do
  exists <- doesFileExist configFileName
  unless exists $ die missingErrMsg
  result <- eitherDecodeFileStrict configFileName
  either (die . parseErrMsg) pure result
  where
    missingErrMsg = configFileName ++ " not found"
    parseErrMsg err = "Failed to parse " ++ configFileName ++ ": " ++ err
