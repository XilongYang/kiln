module UT.Modules.Builder (suiteName, testCases) where

import Modules.Builder
import Modules.BuildPlan
import Modules.Post (Post(..), PostMeta(..), parsePost)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

import Control.Monad (when)
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
