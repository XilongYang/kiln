{-# LANGUAGE OverloadedStrings #-}

module Kiln.ConfigSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Kiln.Config
import Kiln.TestUtil (withTempDir)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Hspec

validJson :: ByteString
validJson =
  "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
  \\"opt\":{\"webroot\":\"/\",\"toc\":{\"enable\":true,\"depth\":3}},\
  \\"target\":{\"searchdb\":\"searchdb\",\"cache-dir\":\"cache\",\
  \\"fonts\":{\"subset-dir\":\"fonts-subset\",\"sources\":[\"fonts/a.ttf\"]},\
  \\"post\":{\"template\":\"post.html\",\"output-dir\":\"post\"},\
  \\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

expectedConfig :: KilnConfig
expectedConfig =
  KilnConfig
    { configInput = InputConfig "src" "template"
    , configOpt = OptConfig "/" (TocConfig True 3)
    , configTarget =
        TargetConfig
          { targetSearchDb = Just "searchdb"
          , targetCacheDir = "cache"
          , targetPost = Just (PostPageConfig "post.html" "post")
          , targetPages = [PageConfig "index.html" "index.html"]
          , targetFonts = Just (FontsConfig "fonts-subset" ["fonts/a.ttf"])
          }
    }

spec :: Spec
spec = do
  describe "KilnConfig JSON parsing" $ do
    it "parses a well-formed config" $ do
      case eitherDecode validJson of
        Left err -> expectationFailure err
        Right cfg -> do
          inputSrcDir (configInput cfg) `shouldBe` "src"
          targetCacheDir (configTarget cfg) `shouldBe` "cache"
          optWebroot (configOpt cfg) `shouldBe` "/"

    it "rejects a config missing required fields" $ do
      case eitherDecode "{\"input\":{}}" :: Either String KilnConfig of
        Left _ -> pure ()
        Right cfg -> expectationFailure ("expected failure, got " ++ show cfg)

    it "parses \"post\": null as no standalone post pages" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":\"searchdb\",\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> targetPost (configTarget cfg) `shouldBe` Nothing

    it "parses \"fonts\": null as no local fonts to subset" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":\"searchdb\",\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> targetFonts (configTarget cfg) `shouldBe` Nothing

    it "parses \"searchdb\": null as no search index to generate" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> targetSearchDb (configTarget cfg) `shouldBe` Nothing

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
