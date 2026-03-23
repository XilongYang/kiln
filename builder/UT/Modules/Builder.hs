module UT.Modules.Builder (suiteName, testCases) where

import Modules.Builder
import Modules.BuildPlan
import Modules.Post (Post(..), PostMeta(..), parsePost)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , removeFile
  )
import System.FilePath ((</>))

mockTempDir :: FilePath
mockTempDir = "builder/UT/.mock/temp"

mockPostDir :: FilePath
mockPostDir = "builder/UT/.mock/post"

mockPostTemplatePath :: FilePath
mockPostTemplatePath = mockTempDir </> "post.html"

mockIndexTemplatePath :: FilePath
mockIndexTemplatePath = mockTempDir </> "index.html"

mockIndexOutputPath :: FilePath
mockIndexOutputPath = mockPostDir </> "builder-ut-index.html"

-- Suite for markdown preprocessing and build-plan execution helpers.
suiteName :: String
suiteName = "Builder"

testCases :: [TestCase]
testCases =
  [ mkTestCase "rewriteLanguageMarkLine rewrites fenced code with language marker" $ do
    assertEq "language fence should be rewritten with classes"
        "``` {.language-haskell .line-numbers .match-braces}"
        (rewriteLanguageMarkLine "```haskell")
  , mkTestCase "rewriteLanguageMarkLine keeps plain fence unchanged" $ do
      assertEq "plain code fence should remain unchanged"
        "```"
        (rewriteLanguageMarkLine "```")
  , mkTestCase "rewriteLanguageMarkLine keeps non-fence line unchanged" $ do
      assertEq "non-fence content should pass through"
        "hello"
        (rewriteLanguageMarkLine "hello")
  , mkTestCase "rewriteLanguageMarks rewrites only fence opener lines" $ do
      let input = unlines ["before", "```python", "print('x')", "```", "after"]
      let expected =
            unlines
              [ "before"
              , "``` {.language-python .line-numbers .match-braces}"
              , "print('x')"
              , "```"
              , "after"
              ]
      assertEq "multi-line rewrite should only transform language fence opener"
        expected
        (rewriteLanguageMarks input)
  , mkTestCase "genPreprocessedPostText includes meta abstract toc and rewritten body" $ do
      let post =
            Post
              { postName = "demo"
              , postSourcePath = "builder/UT/.mock/src/demo.md"
              , postContent = "```js\nconst x = 1;\n```"
              , postAbstract = "short abstract"
              , postMeta = PostMeta "T" "A" "2026-03-22"
              }
      let generated = genPreprocessedPostText post
      assertContains "preprocessed text should contain serialized title" "title: T" generated
      assertContains "preprocessed text should contain abstract wrapper start" "<div class=\"abstract\">" generated
      assertContains "preprocessed text should contain toc marker" "[[toc]]" generated
      assertContains "preprocessed text should rewrite language fence in body"
        "``` {.language-js .line-numbers .match-braces}"
        generated
  , mkTestCase "mkPandocArgs builds deterministic pandoc arguments" $ do
      assertEq "pandoc args should preserve input/template/output positions and flags"
        [ "input.md"
        , "-o", "output.html"
        , "--template=template.html"
        , "--no-highlight"
        , "--mathjax"
        , "--wrap=none"
        ]
        (mkPandocArgs "input.md" "template.html" "output.html")
  , mkTestCase "buildPostWithPlan runs real pandoc and writes html under .mock/post" $
      withFreshMockFile outputPath $
      withFreshMockFile htmlPath $ do
        createDirectoryIfMissing True mockTempDir
        createDirectoryIfMissing True mockPostDir
        post <- parsePost "builder/UT/.mock/src/parse-post-fixture.md"
        let basePlan = expectPostPlan (mkBuildPostPlan post)
        let plan = basePlan { planPreprocessedPath = outputPath, planTargetHtmlPath = htmlPath, planPostTemplatePath = mockPostTemplatePath}
        buildPostWithPlan plan
        exists <- doesFileExist outputPath
        assertTrue "buildPostWithPlan should create preprocess markdown file" exists
        written <- readFile outputPath
        assertContains "written preprocess file should include front matter title" "title: Fixture Title" written
        assertContains "written preprocess file should include toc marker" "[[toc]]" written
        assertContains "written preprocess file should include rewritten C language fence"
          "``` {.language-C .line-numbers .match-braces}"
          written
        htmlExists <- doesFileExist htmlPath
        assertTrue "buildPostWithPlan should create rendered html output" htmlExists
        html <- readFile htmlPath
        assertContains "rendered html should include a heading generated from markdown content" "<h2 id=\"sub-title2\">" html
        assertContains "rendered html should include wrapped abstract block from source" "<div class=\"abstract\">" html
        assertContains "rendered html should preserve rewritten code block classes"
          "<pre class=\"language-C line-numbers match-braces\">"
          html
  , mkTestCase "genIndexHtml groups items by year and orders dates descending in a year" $ do
      let items =
            [ IndexItem "B-Old" "2025" "01" "02" "/post/b-old.html"
            , IndexItem "A-New" "2026" "12" "25" "/post/a-new.html"
            , IndexItem "A-Old" "2026" "02" "03" "/post/a-old.html"
            ]
      let rendered = genIndexHtml items "<main>$posts$</main>"
      assertContains "index html should include 2026 year heading" "<h3>2026</h3>" rendered
      assertContains "index html should include 2025 year heading" "<h3>2025</h3>" rendered
      assertTrue "newer year group should appear before older year group"
        (indexOf "<h3>2026</h3>" rendered < indexOf "<h3>2025</h3>" rendered)
      assertTrue "items in same year should be ordered by date descending"
        (indexOf "A-New" rendered < indexOf "A-Old" rendered)
  , mkTestCase "buildIndexWithPlan writes replaced index html to target file" $
      withFreshMockFile mockIndexTemplatePath $
      withFreshMockFile mockIndexOutputPath $ do
        createDirectoryIfMissing True mockTempDir
        createDirectoryIfMissing True mockPostDir
        writeFile mockIndexTemplatePath "<html><body>$posts$</body></html>"
        let plan =
              IndexBuildPlan
                { planIndexItems =
                    [ IndexItem "Index Title" "2026" "03" "22" "/post/index-title.html" ]
                , planIndexHtmlPath = mockIndexOutputPath
                , planIndexTemplatePath = mockIndexTemplatePath
                , planIndexUrl = "/index.html"
                }
        buildIndexWithPlan plan
        exists <- doesFileExist mockIndexOutputPath
        assertTrue "buildIndexWithPlan should create index output html file" exists
        html <- readFile mockIndexOutputPath
        assertContains "index output should include year heading from items" "<h3>2026</h3>" html
        assertContains "index output should include item link title" "Index Title" html
        assertContains "index output should include item link url" "/post/index-title.html" html
  ]
  where
    outputPath = mockTempDir </> "builder-ut-preprocessed.md"
    htmlPath = mockPostDir </> "builder-ut-output.html"

withFreshMockFile :: FilePath -> IO a -> IO a
withFreshMockFile path action = do
  exists <- doesFileExist path
  when exists (removeFile path)
  action

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"

indexOf :: String -> String -> Int
indexOf needle haystack = go 0 haystack
  where
    go n rest
      | needle `isPrefixOf` rest = n
      | null rest = maxBound
      | otherwise = go (n + 1) (drop 1 rest)
