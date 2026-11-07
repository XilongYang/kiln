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
inputOptPrefix webroot = inputOptFields ("\"webroot\":\"" ++ webroot ++ "\"")

-- | `inputOptPrefix`, plus the origin only a feed page needs.
inputOptWithOrigin :: String -> String -> String
inputOptWithOrigin webroot siteUrl =
  inputOptFields ("\"webroot\":\"" ++ webroot ++ "\",\"site-url\":\"" ++ siteUrl ++ "\"")

inputOptFields :: String -> String
inputOptFields opts =
  "\"input\":{\"src-dir\":\"src\",\"template-dir\":\"template\"},\
  \\"opt\":{" ++ opts ++ "},"

postConfigField :: String
postConfigField = "\"post\":{\"template\":\"post.html\",\"output-dir\":\"post\"},"

configJson :: String
configJson =
  "{" ++ inputOptPrefix "/blog/" ++
  "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

-- | `configJson` with pandoc's own highlighter left on, as a template
-- that loads no Prism wants it.
configJsonNoPrism :: String
configJsonNoPrism =
  "{" ++ inputOptFields "\"webroot\":\"/blog/\",\"prism\":false" ++
  "\"target\":{\"searchdb\":\"searchdb.json\",\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"}]}}"

-- | `configJson`, plus a second page with its own output, so pages are
-- proven to be rendered generically rather than as "the index".
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

-- | A feed template: what distinguishes one is that the post urls and
-- the urls inside the post bodies must both be absolute.
feedTemplate :: String
feedTemplate =
  "<feed>\n\
  \$for(posts)$\n\
  \<entry><id>$posts.url$</id><content>$posts.content$</content></entry>\n\
  \$endfor$\n\
  \</feed>"

-- | `configJson`, plus a feed asking for absolute urls beside an index
-- that doesn't, so the setting is proven to be the page's own.
configJsonWithFeed :: String
configJsonWithFeed =
  "{" ++ inputOptWithOrigin "/blog/" "https://example.com" ++
  "\"target\":{\"searchdb\":null,\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
  "\"pages\":[{\"template\":\"index.html\",\"output\":\"index.html\"},\
  \{\"template\":\"atom.xml\",\"output\":\"atom.xml\",\"absolute-urls\":true}]}}"

-- | `setUpProject`, with a feed and a post carrying both urls a feed
-- has to tell apart: one root-relative, one already absolute.
setUpProjectWithFeed :: IO ()
setUpProjectWithFeed = do
  setUpProject
  writeFile "kiln-config.json" configJsonWithFeed
  writeFile "template/atom.xml" feedTemplate
  writeFile "src/Newer_Post.md"
    (postMd "Newer Post" "2024-05-01"
      "![](/res/a.png)\n\n[pandoc](https://pandoc.org)")

notFoundTemplate :: String
notFoundTemplate =
  "<html><body>${ component/navbar() }\n\
  \<h1>Not Found</h1>\n\
  \</body></html>"

-- | A title long enough that pandoc's default wrapping would break the
-- line it lands on.
longTitle :: String
longTitle = "A title long enough that pandoc would otherwise wrap the line it lands on"

postMd :: String -> String -> String -> String
postMd title date body =
  "---\ntitle: " ++ title ++ "\ndate: " ++ date ++ "\n---\n\n" ++ body

data SearchEntry = SearchEntry {seTitle :: String, seUrl :: Maybe String, seContent :: String}

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

-- | `setUpProject`, plus a real font from the shipped default template,
-- so `subsetFonts` has something to subset.
setUpProjectWithFont :: IO ()
setUpProjectWithFont = do
  setUpProject
  writeFile "kiln-config.json" configJsonWithFont
  createDirectoryIfMissing True "fonts"
  fontSrc <- getDataFileName ("template/default/res/fonts/" ++ fontFileName)
  copyFile fontSrc ("fonts/" ++ fontFileName)

-- | Run `action` with `exe`'s directory off @PATH@: a build that invokes
-- `exe` at all then fails, which proves a cache hit without inspecting
-- logs or stubbing the executable out.
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
        -- Abstract and full content both reach the page, as the HTML
        -- they already were.
        indexHtml `shouldSatisfy` ("abstract=[<p>Newer intro.</p>]" `isInfixOf`)
        indexHtml `shouldSatisfy` ("content=[<p>Newer body.</p>]" `isInfixOf`)
        -- A post with no <!--more--> marker has no abstract at all.
        indexHtml `shouldSatisfy` ("abstract=[] content=[<p>Older body.</p>]" `isInfixOf`)
        -- Newer post must be listed before the older one (same year, newest first).
        let Just newerPos = substringIndex "/blog/post/Newer_Post.html" indexHtml
            Just olderPos = substringIndex "/blog/post/Older_Post.html" indexHtml
        newerPos `shouldSatisfy` (< olderPos)

        searchDb <- either fail pure =<< eitherDecodeFileStrict "searchdb.json"
        length (searchDb :: [SearchEntry]) `shouldBe` 2
        case find ((== "Newer Post") . seTitle) searchDb of
          Nothing -> expectationFailure "searchdb.json is missing the \"Newer Post\" entry"
          Just e -> do
            seUrl e `shouldBe` Just "/blog/post/Newer_Post.html"
            seContent e `shouldSatisfy` ("Newer body." `isInfixOf`)

        -- Scratch space stays inside the cache dir the config declares,
        -- and does not outlive the build.
        doesDirectoryExist ".temp" `shouldReturn` False
        doesDirectoryExist ".cache/work" `shouldReturn` False
        doesDirectoryExist ".cache" `shouldReturn` True

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

  it "hands a page its posts list as data, not as markdown to read again" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        writeFile "src/Dash--Post.md"
          (postMd longTitle "2024-06-01" "Run `--flag \"q\" *stars*` here.")
        kilnBuild

        indexHtml <- readFile "index.html"
        -- A slug's own dashes survive: en-dashed, the link 404s.
        indexHtml `shouldSatisfy` ("\"/blog/post/Dash--Post.html\"" `isInfixOf`)
        -- Rendered HTML arrives as written: no en dash, no curly quote,
        -- no emphasis found inside a code span.
        indexHtml `shouldSatisfy` ("<code>--flag \"q\" *stars*</code>" `isInfixOf`)
        -- Nor is anything rewrapped at pandoc's 72 columns.
        indexHtml `shouldSatisfy` ((">" ++ longTitle ++ "</a>") `isInfixOf`)

  it "leaves fence highlighting to Prism when it is on, to pandoc when it is off" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProject
        writeFile "src/Fenced_Post.md"
          (postMd "Fenced Post" "2024-07-01" "```haskell\nmain = pure ()\n```")
        kilnBuild

        prismHtml <- readFile "post/Fenced_Post.html"
        prismHtml
          `shouldSatisfy` ( "<pre class=\"language-haskell line-numbers match-braces\">\
                            \<code>main = pure ()</code></pre>"
                              `isInfixOf`
                          )
        prismHtml `shouldSatisfy` ("sourceCode" `notIsInfixOf`)

        writeFile "kiln-config.json" configJsonNoPrism
        kilnBuild
        pandocHtml <- readFile "post/Fenced_Post.html"
        pandocHtml `shouldSatisfy` ("class=\"sourceCode haskell\"" `isInfixOf`)

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

  it "renders a feed page with absolute urls, leaving the same build's index relative" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFeed
        kilnBuild

        feed <- readFile "atom.xml"
        feed `shouldSatisfy` ("<id>https://example.com/blog/post/Newer_Post.html</id>" `isInfixOf`)
        -- The body's own urls: root-relative joined onto the origin, the
        -- external one left as written.
        feed `shouldSatisfy` ("src=\"https://example.com/res/a.png\"" `isInfixOf`)
        feed `shouldSatisfy` ("href=\"https://pandoc.org\"" `isInfixOf`)
        feed `shouldSatisfy` ("src=\"/res/a.png\"" `notIsInfixOf`)

        -- Same posts, same build, a page that didn't ask: still relative.
        indexHtml <- readFile "index.html"
        indexHtml `shouldSatisfy` ("href=\"/blog/post/Newer_Post.html\"" `isInfixOf`)
        indexHtml `shouldSatisfy` ("src=\"/res/a.png\"" `isInfixOf`)
        indexHtml `shouldSatisfy` ("https://example.com" `notIsInfixOf`)

  it "re-renders a feed whose site-url changed, with nothing else touched" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFeed
        kilnBuild
        writeFile "kiln-config.json"
          ("{" ++ inputOptWithOrigin "/blog/" "https://elsewhere.example" ++
          "\"target\":{\"searchdb\":null,\"cache-dir\":\".cache\",\"fonts\":null," ++ postConfigField ++
          "\"pages\":[{\"template\":\"atom.xml\",\"output\":\"atom.xml\",\"absolute-urls\":true}]}}")
        withoutPandoc kilnBuild `shouldThrow` anyIOException

  it "serves an unchanged font subset from cache, without needing pyftsubset again" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        withoutPyftsubset kilnBuild

        doesFileExist "fonts-subset/JetBrainsMono-Regular.woff2" `shouldReturn` True

  it "re-subsets every font when a page brings a character the subset lacks" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        writeFile "src/Older_Post.md" (postMd "Older Post" "2024-01-01" "Older body, 漢.")
        withoutPyftsubset kilnBuild `shouldThrow` anyIOException

  it "serves the subset from cache when a changed page brings no new character" $
    withTempDir $ \dir ->
      withCurrentDirectory dir $ do
        setUpProjectWithFont
        kilnBuild
        writeFile "src/Older_Post.md" (postMd "Older Post" "2024-01-01" "Older body. Older body.")
        withoutPyftsubset kilnBuild

        -- The post really was re-rendered: only the subset was skipped.
        olderHtml <- readFile "post/Older_Post.html"
        olderHtml `shouldSatisfy` ("Older body. Older body." `isInfixOf`)
        doesFileExist "fonts-subset/JetBrainsMono-Regular.woff2" `shouldReturn` True

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
