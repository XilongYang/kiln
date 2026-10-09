{-# LANGUAGE OverloadedStrings #-}

module Kiln.Config
  ( KilnConfig (..)
  , PathConfig (..)
  , InPaths (..)
  , OutPaths (..)
  , TocConfig (..)
  , PageConfig (..)
  , PostPageConfig (..)
  , readConfig
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?), (.!=))
import Data.Map.Strict (Map)
import System.Directory (doesFileExist)
import System.Exit (die)

data InPaths = InPaths
  { inSrc      :: FilePath
  , inTemplate :: FilePath
  , inFonts    :: FilePath
  } deriving (Show, Eq)

data OutPaths = OutPaths
  { outFontsSubset :: FilePath
  , outSearchDb    :: FilePath
  , outCache       :: FilePath
  } deriving (Show, Eq)

-- | One page to generate: its own template (a path under
-- @path.in.template@), and the site-relative path to write it to. A
-- page's `pageOutput` doubles as its own identity -- two pages can't
-- share one without one silently overwriting the other's output
-- regardless of caching, so it's already a safe, unique key for the
-- page's own build cache; there's no separate name to keep in sync.
data PageConfig = PageConfig
  { pageTemplate :: FilePath
  , pageOutput   :: FilePath
  } deriving (Show, Eq)

-- | The per-post standalone page @kiln build@ renders for every
-- markdown file in @path.in.src@, if any -- a site like a flat
-- card-flow feed may not want one at all (see `Kiln.Config.readConfig`'s
-- caller, which requires this to be stated explicitly as either this or
-- JSON @null@, never silently defaulted). `postPageOutput` is a
-- directory; a post with slug @s@ is written to
-- @postPageOutput </> s <> ".html"@.
data PostPageConfig = PostPageConfig
  { postPageTemplate :: FilePath
  , postPageOutput   :: FilePath
  } deriving (Show, Eq)

data PathConfig = PathConfig
  { pathIn  :: InPaths
  , pathOut :: OutPaths
  } deriving (Show, Eq)

data TocConfig = TocConfig
  { tocEnable          :: Bool
  , tocDepth           :: Int
  } deriving (Show, Eq)

data KilnConfig = KilnConfig
  { configPath    :: PathConfig
  , configWebroot :: FilePath
  , configFonts   :: Map String String
    -- ^ local font filename (under @path.in.fonts@) -> the @font-family@
    -- name it's declared under in the site's own CSS.
  , configToc     :: TocConfig
  , configPost    :: Maybe PostPageConfig
  , configPages   :: [PageConfig]
  } deriving (Show, Eq)

instance FromJSON InPaths where
  parseJSON = withObject "in" $ \o ->
    InPaths
      <$> o .: "src"
      <*> o .: "template"
      <*> o .: "fonts"

instance FromJSON OutPaths where
  parseJSON = withObject "out" $ \o ->
    OutPaths
      <$> o .: "fonts-subset"
      <*> o .: "searchdb"
      <*> o .: "cache"

instance FromJSON PageConfig where
  parseJSON = withObject "page" $ \o ->
    PageConfig
      <$> o .: "template"
      <*> o .: "output"

instance FromJSON PostPageConfig where
  parseJSON = withObject "post" $ \o ->
    PostPageConfig
      <$> o .: "template"
      <*> o .: "output"

instance FromJSON PathConfig where
  parseJSON = withObject "path" $ \o ->
    PathConfig
      <$> o .: "in"
      <*> o .: "out"

instance FromJSON TocConfig where
  parseJSON = withObject "toc" $ \o ->
    TocConfig
      <$> o .:? "enable" .!= True
      <*> o .:? "depth" .!= 3

instance FromJSON KilnConfig where
  parseJSON = withObject "kiln-config" $ \o ->
    KilnConfig
      <$> o .: "path"
      <*> o .: "webroot"
      <*> o .: "fonts"
      <*> o .:? "toc" .!= TocConfig True 3
      <*> o .: "post"
      <*> o .: "pages"

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
