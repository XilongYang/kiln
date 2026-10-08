{-# LANGUAGE OverloadedStrings #-}

module Kiln.BuildSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:))
import Data.List (find, intercalate, isInfixOf, isPrefixOf, tails)
import Kiln.Build (kilnBuild)
import Kiln.TestUtil (withTempDir)
import Paths_kiln (getDataFileName)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , removeFile
  , withCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv)
import System.FilePath (searchPathSeparator, splitSearchPath, takeDirectory)
import Test.Hspec

configJson :: String
configJson =
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
  \\"out\":{\"post\":\"post\",\"fonts-subset\":\"fonts-subset\",\"searchdb\":\"searchdb.json\",\
  \\"index\":\"index.html\",\"cache\":\".cache\"}},\"webroot\":\"/blog/\",\"fonts\":{}}"

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

fontFileName :: String
fontFileName = "JetBrainsMono-Regular.ttf"

configJsonWithFont :: String
configJsonWithFont =
  "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
  \\"out\":{\"post\":\"post\",\"fonts-subset\":\"fonts-subset\",\"searchdb\":\"searchdb.json\",\
  \\"index\":\"index.html\",\"cache\":\".cache\"}},\"webroot\":\"/blog/\",\
  \\"fonts\":{\"" ++ fontFileName ++ "\":\"Mono\"}}"

-- | `setUpProject`, plus a real font (borrowed from the shipped
-- hello-kiln template, via the same `Paths_kiln` data-dir lookup `Init`
-- uses) declared in `kiln-config.json`, so `subsetFonts` has something
-- to actually subset.
setUpProjectWithFont :: IO ()
setUpProjectWithFont = do
  setUpProject
  writeFile "kiln-config.json" configJsonWithFont
  createDirectoryIfMissing True "fonts"
  fontSrc <- getDataFileName ("template/hello-kiln/res/fonts/" ++ fontFileName)
  copyFile fontSrc ("fonts/" ++ fontFileName)

-- | Run `action` with `exe`'s directory removed from @PATH@, so that any
-- step of `kilnBuild` that actually tries to invoke `exe` (i.e. any
-- cache miss affecting it) fails instead of silently succeeding -- the
-- cleanest way to prove a given build served everything from cache
-- without having to inspect logs or stub the executable out.
withoutExecutable :: String -> IO a -> IO a
withoutExecutable exe action = do
  original <- lookupEnv "PATH"
  exePath <- findExecutable exe
  let exeDir = takeDirectory <$> exePath
      dirs = maybe [] splitSearchPath original
      stripped = intercalate [searchPathSeparator] (filter (\d -> Just d /= exeDir) dirs)
  bracket
    (setEnv "PATH" stripped)
    (const (maybe (pure ()) (setEnv "PATH") original))
    (const action)

withoutPandoc :: IO a -> IO a
withoutPandoc = withoutExecutable "pandoc"

withoutPyftsubset :: IO a -> IO a
withoutPyftsubset = withoutExecutable "pyftsubset"

spec :: Spec
spec = describe "kilnBuild" $ do
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

  it "serves an unchanged post from cache, without needing pandoc again" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        kilnBuild
        withoutPandoc kilnBuild

        olderHtml <- readFile "post/Older_Post.html"
        olderHtml `shouldSatisfy` ("Older body." `isInfixOf`)

  it "re-renders a post whose source changed, even with everything else untouched" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        kilnBuild
        writeFile "src/Older_Post.md" (postMd "Older Post" "2024-01-01" "Edited body.")
        withoutPandoc kilnBuild `shouldThrow` anyIOException

  it "re-renders every post when a site-wide setting (webroot) changes" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        kilnBuild
        writeFile "kiln-config.json"
          "{\"path\":{\"in\":{\"src\":\"src\",\"template\":\"template\",\"fonts\":\"fonts\"},\
          \\"out\":{\"post\":\"post\",\"fonts-subset\":\"fonts-subset\",\"searchdb\":\"searchdb.json\",\
          \\"index\":\"index.html\",\"cache\":\".cache\"}},\"webroot\":\"/elsewhere/\",\"fonts\":{}}"
        withoutPandoc kilnBuild `shouldThrow` anyIOException

  it "warns about, but keeps, an orphaned post; only drops its cache once the output is gone too" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        kilnBuild
        removeFile "src/Older_Post.md"
        kilnBuild

        doesFileExist "post/Older_Post.html" `shouldReturn` True
        doesFileExist ".cache/stage/Older_Post.src" `shouldReturn` True

        removeFile "post/Older_Post.html"
        kilnBuild

        doesFileExist ".cache/stage/Older_Post.src" `shouldReturn` False
        doesFileExist ".cache/items/Older_Post.json" `shouldReturn` False
        doesFileExist ".cache/search/Older_Post.txt" `shouldReturn` False

  it "serves an unchanged font subset from cache, without needing pyftsubset again" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        withoutPyftsubset kilnBuild

        doesFileExist "fonts-subset/JetBrainsMono-Regular.woff2" `shouldReturn` True

  it "re-subsets every font when a rendered page's content changes" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        writeFile "src/Older_Post.md" (postMd "Older Post" "2024-01-01" "Edited body with new words.")
        withoutPyftsubset kilnBuild `shouldThrow` anyIOException

  it "re-subsets every font when the font file itself changes" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        appendFile ("fonts/" ++ fontFileName) "\0" -- any byte change is enough
        withoutPyftsubset kilnBuild `shouldThrow` anyIOException
  where
    notIsInfixOf needle haystack = not (needle `isInfixOf` haystack)

substringIndex :: String -> String -> Maybe Int
substringIndex needle haystack = go 0 (tails haystack)
  where
    go _ [] = Nothing
    go i (t : ts)
      | needle `isPrefixOf` t = Just i
      | otherwise = go (i + 1) ts
