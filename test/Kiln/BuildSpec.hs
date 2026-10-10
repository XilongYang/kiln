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

inputOptPrefix :: String -> String
inputOptPrefix webroot =
  "\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
  \\"opt\":{\"webroot\":\"" ++ webroot ++ "\"},"

postConfigField :: String
postConfigField = "\"post\":{\"template\":\"post.html\",\"output-dir\":\"post\"},"

configJson :: String
configJson =
  "{" ++ inputOptPrefix "/blog/" ++
  "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

-- | `configJson`, plus a second configured page (a stand-in for a 404
-- page, with a distinct output to prove `kilnBuild` renders every
-- configured page generically, not just one hardcoded as "the index").
configJsonWithExtraPage :: String
configJsonWithExtraPage =
  "{" ++ inputOptPrefix "/blog/" ++
  "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"},\
  \{\"template\":\"404.html\",\"output\":\"404.html\"}]}}"

indexTemplate :: String
indexTemplate =
  "<html><body>${ component/navbar() }\n\
  \<div id=\"posts_wrapper\">\n\
  \$for(posts)$\n\
  \$if(posts.newYear)$<h3>$posts.year$</h3>$endif$\n\
  \<p><a href=\"$posts.url$\">$posts.title$</a> abstract=[$posts.abstract$] content=[$posts.content$]</p>\n\
  \$endfor$\n\
  \</div></body></html>"

postTemplate :: String
postTemplate =
  "<html><head><title>$title$</title></head><body>${ component/navbar() }\n\
  \<h1>$title$</h1><p>$date$</p>\n\
  \$body$\n\
  \</body></html>"

navbarComponent :: String
navbarComponent = "<nav><a href=\"$webroot$\">THE-NAVBAR</a></nav>"

notFoundTemplate :: String
notFoundTemplate =
  "<html><body>${ component/navbar() }\n\
  \<h1>Not Found</h1>\n\
  \</body></html>"

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
  writeFile "src/Newer_Post.md" (postMd "Newer Post" "2024-05-01" "Newer intro.\n\n<!--more-->\n\nNewer body.")

fontFileName :: String
fontFileName = "JetBrainsMono-Regular.ttf"

configJsonWithFont :: String
configJsonWithFont =
  "{" ++ inputOptPrefix "/blog/" ++
  "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\
  \\"fonts\":{\"subset-dir\":\"fonts-subset\",\"sources\":[\"fonts/" ++ fontFileName ++ "\"]}," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

-- | `setUpProject`, plus a real font (borrowed from the shipped
-- default template, via the same `Paths_kiln` data-dir lookup `Init`
-- uses) declared in `kiln-config.json`, so `subsetFonts` has something
-- to actually subset.
setUpProjectWithFont :: IO ()
setUpProjectWithFont = do
  setUpProject
  writeFile "kiln-config.json" configJsonWithFont
  createDirectoryIfMissing True "fonts"
  fontSrc <- getDataFileName ("template/default/res/fonts/" ++ fontFileName)
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
        indexHtml `shouldSatisfy` ("$for(posts)$" `notIsInfixOf`)
        indexHtml `shouldSatisfy` ("$webroot$" `notIsInfixOf`)
        indexHtml `shouldSatisfy` ("<h3>2024</h3>" `isInfixOf`)
        -- Newer post's abstract (before its <!--more--> marker) and full
        -- content (including the text after it) must both reach the page.
        indexHtml `shouldSatisfy` ("abstract=[<p>\nNewer intro.\n</p>]" `isInfixOf`)
        indexHtml `shouldSatisfy` ("content=[<p>\nNewer body.\n</p>]" `isInfixOf`)
        -- A post with no <!--more--> marker has no abstract at all.
        indexHtml `shouldSatisfy` ("abstract=[] content=[<p>\nOlder body.\n</p>]" `isInfixOf`)
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

  it "renders every configured page, not just the first one" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        writeFile "kiln-config.json" configJsonWithExtraPage
        writeFile "template/404.html" notFoundTemplate
        kilnBuild

        notFoundHtml <- readFile "404.html"
        notFoundHtml `shouldSatisfy` ("<a href=\"/blog/\">THE-NAVBAR</a>" `isInfixOf`)
        notFoundHtml `shouldSatisfy` ("<h1>Not Found</h1>" `isInfixOf`)

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
          ("{" ++ inputOptPrefix "/elsewhere/" ++
          "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
          "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}")
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

  it "doesn't write a search index when \"searchdb\" is null" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        writeFile "kiln-config.json"
          ("{" ++ inputOptPrefix "/blog/" ++
          "\"target\":{\"searchdb\":null,\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
          "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}")
        kilnBuild

        doesFileExist "searchdb.json" `shouldReturn` False

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
