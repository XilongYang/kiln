{-# LANGUAGE OverloadedStrings #-}

module Kiln.BuildSpec (spec) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (find, isInfixOf, isPrefixOf, tails)
import Kiln.Build (kilnBuild)
import Kiln.TestUtil (withTempDir)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , withCurrentDirectory
  )
import Test.Hspec

configJson :: String
configJson =
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
  \\"out\":{\"post\":\"post\",\"fonts-subset\":\"fonts-subset\",\"searchdb\":\"searchdb.json\",\
  \\"index\":\"index.html\",\"cache\":\".cache\"}},\"webroot\":\"/blog/\"}"

indexTemplate :: String
indexTemplate =
  "<html><body><!--<navbar>-->\n\
  \<div id=\"posts_wrapper\">\n\
  \$posts$\n\
  \</div></body></html>"

postTemplate :: String
postTemplate =
  "<html><head><title>$title$</title></head><body><!--<navbar>-->\n\
  \<h1>$title$</h1><p>$date$</p>\n\
  \$body$\n\
  \</body></html>"

navbarComponent :: String
navbarComponent = "<nav><a href=\"$webroot$\">THE-NAVBAR</a></nav>"

postMd :: String -> String -> String -> String
postMd title date body =
  "---\ntitle: " ++ title ++ "\ndate: " ++ date ++ "\n---\n\n" ++ body

data SearchEntry = SearchEntry {seTitle :: String, seUrl :: String, seContent :: String}

instance FromJSON SearchEntry where
  parseJSON = withObject "search-entry" $ \o ->
    SearchEntry <$> o .: "title" <*> o .: "url" <*> o .: "content"

setUpProject :: IO ()
setUpProject = do
  writeFile "kiln-config.json" configJson
  createDirectoryIfMissing True "template/component"
  writeFile "template/index.html" indexTemplate
  writeFile "template/post.html" postTemplate
  writeFile "template/component/navbar.html" navbarComponent
  createDirectoryIfMissing True "src"
  writeFile "src/Older_Post.md" (postMd "Older Post" "2024-01-01" "Older body.")
  writeFile "src/Newer_Post.md" (postMd "Newer Post" "2024-05-01" "Newer body.")

spec :: Spec
spec = describe "kilnBuild" $
  it "renders posts and the index from markdown, components and templates" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        kilnBuild

        olderHtml <- readFile "post/Older_Post.html"
        olderHtml `shouldSatisfy` ("<a href=\"/blog/\">THE-NAVBAR</a>" `isInfixOf`)
        olderHtml `shouldSatisfy` ("<h1>Older Post</h1>" `isInfixOf`)
        olderHtml `shouldSatisfy` ("Older body." `isInfixOf`)

        newerHtml <- readFile "post/Newer_Post.html"
        newerHtml `shouldSatisfy` ("<h1>Newer Post</h1>" `isInfixOf`)

        indexHtml <- readFile "index.html"
        indexHtml `shouldSatisfy` ("<a href=\"/blog/\">THE-NAVBAR</a>" `isInfixOf`)
        indexHtml `shouldSatisfy` ("$posts$" `notIsInfixOf`)
        indexHtml `shouldSatisfy` ("$webroot$" `notIsInfixOf`)
        indexHtml `shouldSatisfy` ("<h3>2024</h3>" `isInfixOf`)
        -- Newer post must be listed before the older one (same year, newest first).
        let Just newerPos = substringIndex "/blog/post/Newer_Post.html" indexHtml
            Just olderPos = substringIndex "/blog/post/Older_Post.html" indexHtml
        newerPos `shouldSatisfy` (< olderPos)

        searchDb <- either fail pure =<< eitherDecodeFileStrict "searchdb.json"
        length (searchDb :: [SearchEntry]) `shouldBe` 2
        case find ((== "Newer Post") . seTitle) searchDb of
          Nothing -> expectationFailure "searchdb.json is missing the \"Newer Post\" entry"
          Just e -> do
            seUrl e `shouldBe` "/blog/post/Newer_Post.html"
            seContent e `shouldSatisfy` ("Newer body." `isInfixOf`)

        doesDirectoryExist ".temp" `shouldReturn` False
  where
    notIsInfixOf needle haystack = not (needle `isInfixOf` haystack)

substringIndex :: String -> String -> Maybe Int
substringIndex needle haystack = go 0 (tails haystack)
  where
    go _ [] = Nothing
    go i (t : ts)
      | needle `isPrefixOf` t = Just i
      | otherwise = go (i + 1) ts
