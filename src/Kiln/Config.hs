{-# LANGUAGE OverloadedStrings #-}

module Kiln.Config
  ( KilnConfig (..)
  , PathConfig (..)
  , InPaths (..)
  , OutPaths (..)
  , readConfig
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
import System.Directory (doesFileExist)
import System.Exit (die)

data InPaths = InPaths
  { inSrc      :: FilePath
  , inTemplate :: FilePath
  , inFonts    :: FilePath
  } deriving Show

data OutPaths = OutPaths
  { outPost        :: FilePath
  , outFontsSubset :: FilePath
  , outSearchDb    :: FilePath
  , outIndex       :: FilePath
  , outCache       :: FilePath
  } deriving Show

data PathConfig = PathConfig
  { pathIn  :: InPaths
  , pathOut :: OutPaths
  } deriving Show

data KilnConfig = KilnConfig
  { configPath    :: PathConfig
  , configWebroot :: FilePath
  } deriving Show

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
      <*> o .: "index"
      <*> o .: "cache"

instance FromJSON PathConfig where
  parseJSON = withObject "path" $ \o ->
    PathConfig
      <$> o .: "in"
      <*> o .: "out"

instance FromJSON KilnConfig where
  parseJSON = withObject "kiln-config" $ \o ->
    KilnConfig
      <$> o .: "path"
      <*> o .: "webroot"

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
