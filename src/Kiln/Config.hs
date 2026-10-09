{-# LANGUAGE OverloadedStrings #-}

module Kiln.Config
  ( KilnConfig (..)
  , PathConfig (..)
  , InPaths (..)
  , OutPaths (..)
  , TocConfig (..)
  , PageConfig (..)
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
  { outPost        :: FilePath
  , outFontsSubset :: FilePath
  , outSearchDb    :: FilePath
  , outCache       :: FilePath
  } deriving (Show, Eq)

-- | One page to generate, outside of the per-post pages @kiln build@
-- already renders: its own template (a path under @path.in.template@,
-- alongside @post.html@ and @component/@), and the site-relative path to
-- write it to. `pageName` is just an identifier -- used to key this
-- page's build cache -- not itself part of the output path.
data PageConfig = PageConfig
  { pageName     :: String
  , pageTemplate :: FilePath
  , pageOutput   :: FilePath
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
      <$> o .: "post"
      <*> o .: "fonts-subset"
      <*> o .: "searchdb"
      <*> o .: "cache"

instance FromJSON PageConfig where
  parseJSON = withObject "page" $ \o ->
    PageConfig
      <$> o .: "name"
      <*> o .: "template"
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
