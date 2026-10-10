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
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\"},\
  \\"out\":{\"searchdb\":\"searchdb\",\
  \\"cache\":\"cache\"}},\"webroot\":\"/\",\
  \\"fonts\":{\"subset-path\":\"fonts-subset\",\"sources\":[\"fonts/a.ttf\"]},\
  \\"post\":{\"template\":\"post.html\",\"output\":\"post\"},\
  \\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}"

expectedConfig :: KilnConfig
expectedConfig =
  KilnConfig
    { configPath =
        PathConfig
          { pathIn = InPaths "src" "template"
          , pathOut = OutPaths "searchdb" "cache"
          }
    , configWebroot = "/"
    , configFonts = Just (FontsConfig "fonts-subset" ["fonts/a.ttf"])
    , configToc = TocConfig True 3
    , configPost = Just (PostPageConfig "post.html" "post")
    , configPages = [PageConfig "index.html" "index.html"]
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

    it "parses \"post\": null as no standalone post pages" $ do
      let json =
            "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\"},\
            \\"out\":{\"searchdb\":\"searchdb\",\
            \\"cache\":\"cache\"}},\"webroot\":\"/\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> configPost cfg `shouldBe` Nothing

    it "parses \"fonts\": null as no local fonts to subset" $ do
      let json =
            "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\"},\
            \\"out\":{\"searchdb\":\"searchdb\",\
            \\"cache\":\"cache\"}},\"webroot\":\"/\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> configFonts cfg `shouldBe` Nothing

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
