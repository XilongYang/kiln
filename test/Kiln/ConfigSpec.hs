{-# LANGUAGE OverloadedStrings #-}

module Kiln.ConfigSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Kiln.Config
import Kiln.TestUtil (withTempDir)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Hspec

validJson :: ByteString
validJson =
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
  \\"out\":{\"post\":\"post\",\"fonts-subset\":\"fonts-subset\",\"searchdb\":\"searchdb\",\
  \\"index\":\"index\",\"cache\":\"cache\"}},\"webroot\":\"/\",\
  \\"fonts\":{\"a.ttf\":\"A\"}}"

expectedConfig :: KilnConfig
expectedConfig =
  KilnConfig
    { configPath =
        PathConfig
          { pathIn = InPaths "src" "template" "fonts"
          , pathOut = OutPaths "post" "fonts-subset" "searchdb" "index" "cache"
          }
    , configWebroot = "/"
    , configFonts = Map.fromList [("a.ttf", "A")]
    , configToc = TocConfig True 3 False
    }

spec :: Spec
spec = do
  describe "KilnConfig JSON parsing" $ do
    it "parses a well-formed config" $ do
      case eitherDecode validJson of
        Left err -> expectationFailure err
        Right cfg -> do
          inSrc (pathIn (configPath cfg)) `shouldBe` "src"
          outCache (pathOut (configPath cfg)) `shouldBe` "cache"
          configWebroot cfg `shouldBe` "/"

    it "rejects a config missing required fields" $ do
      case eitherDecode "{\"path\":{}}" :: Either String KilnConfig of
        Left _ -> pure ()
        Right cfg -> expectationFailure ("expected failure, got " ++ show cfg)

  describe "readConfig" $ do
    it "dies when kiln-config.json is missing" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $
          readConfig `shouldThrow` (== ExitFailure 1)

    it "dies when kiln-config.json contains invalid JSON" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          writeFile "kiln-config.json" "{not valid json"
          readConfig `shouldThrow` (== ExitFailure 1)

    it "parses a well-formed kiln-config.json from disk" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          BL.writeFile "kiln-config.json" validJson
          cfg <- readConfig
          cfg `shouldBe` expectedConfig
