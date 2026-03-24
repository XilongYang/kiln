module UT.Modules.PostBuilder (suiteName, testCases) where

import Modules.Builder.PostBuilder
import Modules.Post (Post(..), PostMeta(..))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for post-builder pure helpers and pandoc argument construction.
suiteName :: String
suiteName = "PostBuilder"

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
  ]
