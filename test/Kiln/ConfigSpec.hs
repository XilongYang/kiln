{-# LANGUAGE OverloadedStrings #-}

module Kiln.ConfigSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
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

-- | A config with `optFields` added to its @opt@ and `pages` as its
-- @target.pages@.
configJson :: String -> String -> ByteString
configJson optFields pages =
  BLC.pack
    ( "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
      \\"opt\":{\"webroot\":\"/\""
        ++ optFields
        ++ "},\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
           \\"fonts\":null,\"post\":null,\"pages\":["
        ++ pages
        ++ "]}}"
    )

-- | `configErrors` on what `configJson` builds.
errorsWith :: String -> String -> Either String [String]
errorsWith optFields pages = configErrors <$> eitherDecode (configJson optFields pages)

feedPage :: String
feedPage = "{\"template\":\"atom.xml\",\"output\":\"atom.xml\",\"absolute-urls\":true}"

originlessFeedErr :: String
originlessFeedErr = "\"atom.xml\" asks for \"absolute-urls\", but opt.\"site-url\" is empty"

expectedConfig :: KilnConfig
expectedConfig =
  KilnConfig
    { configInput = InputConfig "src" "template"
    , configOpt = OptConfig "/" "" (TocConfig True 3 True) True True
    , configTarget =
        TargetConfig
          { targetSearchDb = Just "searchdb"
          , targetCacheDir = "cache"
          , targetPost = Just (PostPageConfig "post.html" "post")
          , targetPages = [PageConfig "index.html" "index.html" False]
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

    it "defaults every optional \"opt\" setting to the bundled default template's behavior" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> configOpt cfg `shouldBe` OptConfig "/" "" (TocConfig True 3 True) True True

    it "parses the \"opt\" settings a template turns off" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\",\"toc\":{\"enable\":false},\
            \\"prism\":false,\"math\":false},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> configOpt cfg `shouldBe` OptConfig "/" "" (TocConfig False 3 True) False False

    it "parses the site's own origin, which only a feed needs" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\",\"site-url\":\"https://example.com\"},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> optSiteUrl (configOpt cfg) `shouldBe` "https://example.com"

    it "parses a page asking for absolute urls, and defaults the rest to site-relative ones" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\
            \\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"},\
            \{\"template\":\"atom.xml\",\"output\":\"atom.xml\",\"absolute-urls\":true}]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg ->
          map pageAbsoluteUrls (targetPages (configTarget cfg)) `shouldBe` [False, True]

    it "parses \"searchdb\": null as no search index to generate" $ do
      let json =
            "{\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
            \\"opt\":{\"webroot\":\"/\"},\
            \\"target\":{\"searchdb\":null,\"cache-dir\":\"cache\",\
            \\"fonts\":null,\"post\":null,\"pages\":[]}}"
      case eitherDecode json of
        Left err -> expectationFailure err
        Right cfg -> targetSearchDb (configTarget cfg) `shouldBe` Nothing

  describe "configErrors" $ do
    it "accepts a feed page on a site that declared an origin" $
      errorsWith ",\"site-url\":\"https://example.com\"" feedPage `shouldBe` Right []

    it "accepts a site with no origin and no page asking for one" $
      errorsWith "" "{\"template\":\"index.html\",\"output\":\"index.html\"}" `shouldBe` Right []

    it "reports a page asking for absolute urls the site has no origin for" $
      errorsWith "" feedPage `shouldBe` Right [originlessFeedErr]

    it "reports every such page, not just the first" $
      errorsWith "" (feedPage ++ ",{\"template\":\"rss.xml\",\"output\":\"rss.xml\",\"absolute-urls\":true}")
        `shouldBe` Right
          [ originlessFeedErr
          , "\"rss.xml\" asks for \"absolute-urls\", but opt.\"site-url\" is empty"
          ]

    it "counts an origin of nothing but slashes as none declared" $
      errorsWith ",\"site-url\":\"/\"" feedPage `shouldBe` Right [originlessFeedErr]

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

    it "dies when a page asks for absolute urls and the site declared no origin" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          BL.writeFile "kiln-config.json" (configJson "" feedPage)
          readConfig `shouldThrow` (== ExitFailure 1)

    it "parses a well-formed kiln-config.json from disk" $
      withTempDir $ \dir ->
        withCurrentDirectory dir $ do
          BL.writeFile "kiln-config.json" validJson
          cfg <- readConfig
          cfg `shouldBe` expectedConfig
