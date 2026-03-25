module UT.Modules.PostBuilder (suiteName, testCases) where

import Data.List (isInfixOf, isPrefixOf)
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
              , postBody = "```js\nconst x = 1;\n```"
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
  , mkTestCase "extractHeadItem parses heading level id and title" $ do
      assertEq "heading line should be decoded into level/id/title tuple"
        (Just ("2", "sub-title1", "Sub Title1"))
        (extractHeadItem "<h2 id=\"sub-title1\">Sub Title1</h2>")
  , mkTestCase "extractHeadItem ignores non-heading lines" $ do
      assertEq "non-heading line should not produce toc item"
        Nothing
        (extractHeadItem "<p>plain text</p>")
  , mkTestCase "tocItems nests sub-headings under previous higher-level heading" $ do
      let rendered = tocItems [("2", "a", "A"), ("3", "a-1", "A-1"), ("2", "b", "B")]
      assertTrue "top-level heading A should appear before its sub-heading"
        (indexOf "#a\">A" rendered < indexOf "#a-1\">A-1" rendered)
      assertTrue "sub-heading should appear before next sibling top-level heading"
        (indexOf "#a-1\">A-1" rendered < indexOf "#b\">B" rendered)
  , mkTestCase "genToc injects toc and only scans headings after <main>" $ do
      let html =
            unlines
              [ "<h2 id=\"head-only\">Head Only</h2>"
              , "<main>"
              , "<h2 id=\"main-title\">Main Title</h2>"
              , "<h3 id=\"main-sub\">Main Sub</h3>"
              , "</main>"
              , "[[toc]]"
              ]
      let rendered = genToc html
      assertContains "toc nav wrapper should be injected" "<nav role=\"navigation\" class=\"toc\">" rendered
      assertContains "main h2 should be present in toc links" "href=\"#main-title\"" rendered
      assertContains "main h3 should be present in toc links" "href=\"#main-sub\"" rendered
      assertFalse "head-only heading before <main> should be excluded from toc"
        ("href=\"#head-only\"" `isInfixOf` rendered)
  ]

indexOf :: String -> String -> Int
indexOf needle haystack = go 0 haystack
  where
    go n rest
      | needle `isPrefixOf` rest = n
      | null rest = maxBound
      | otherwise = go (n + 1) (drop 1 rest)
