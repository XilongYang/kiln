module UT.Modules.Post.Preprocess (suiteName, testCases) where

import Modules.Post (Post(..), PostMeta(..))
import Modules.Post.Preprocess (preprocessPost)
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "Post.Preprocess"

testCases :: [TestCase]
testCases =
  [ testPreprocessPostIncludesMetaAbstractAndToc
  , testPreprocessPostRewritesLanguageFenceOpeners
  ]

testPreprocessPostIncludesMetaAbstractAndToc :: TestCase
testPreprocessPostIncludesMetaAbstractAndToc =
  mkTestCase "preprocessPost includes front matter abstract wrapper and toc marker" $ do
    let post =
          Post
            { postName = "demo"
            , postSourcePath = srcFixtureFile "demo.md"
            , postBody = "Body line"
            , postAbstract = "short abstract"
            , postMeta = PostMeta "T" "A" "2026-03-22"
            }
    let rendered = preprocessPost post
    assertContains "preprocessPost should include serialized title line" "title: T" rendered
    assertContains "preprocessPost should include abstract wrapper start" "<div class=\"abstract\">" rendered
    assertContains "preprocessPost should include toc marker" "[[toc]]" rendered

testPreprocessPostRewritesLanguageFenceOpeners :: TestCase
testPreprocessPostRewritesLanguageFenceOpeners =
  mkTestCase "preprocessPost rewrites language fence openers with classes" $ do
    let post =
          Post
            { postName = "demo"
            , postSourcePath = srcFixtureFile "demo.md"
            , postBody = "```js\nconst x = 1;\n```"
            , postAbstract = "short abstract"
            , postMeta = PostMeta "T" "A" "2026-03-22"
            }
    let rendered = preprocessPost post
    assertContains "language fence should be rewritten with language and highlight classes"
      "``` {.language-js .line-numbers .match-braces}"
      rendered
