module UT.Modules.Post (suiteName, testCases) where

import Modules.Config
import Modules.Post
  ( Post(..)
  , PostMeta(..)
  , metaDelimiter
  , parseMetaLine
  , parsePost
  , parsePostMeta
  , splitFrontMatter
  )
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

import Control.Exception (finally)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>), takeBaseName)
import System.IO (writeFile)

-- Suite for post metadata line parsing.
suiteName :: String
suiteName = "Post"

testCases :: [TestCase]
testCases =
  [ mkTestCase "parseMetaLine returns empty pair for empty input" $
    assertEq "parseMetaLine should return empty key/value on empty input"
        ("", "")
        (parseMetaLine "")
  , mkTestCase "parseMetaLine splits key/value by the first colon" $
      assertEq "parseMetaLine should split on first colon and drop delimiter"
        ("title", "Hello World")
        (parseMetaLine "title:Hello World")
  , mkTestCase "parseMetaLine trims surrounding spaces for both key and value" $
      assertEq "parseMetaLine should trim both sides"
        ("author", "Xilong Yang")
        (parseMetaLine "   author   :   Xilong Yang   ")
  , mkTestCase "parseMetaLine keeps extra colons inside value" $
      assertEq "parseMetaLine should only split on the first colon"
        ("summary", "part1:part2")
        (parseMetaLine "summary:part1:part2")
  , mkTestCase "parseMetaLine throws when colon is missing" $
      assertThrows "parseMetaLine should throw when delimiter is absent" $
        pure (parseMetaLine "  draft   ")
  , mkTestCase "parsePostMeta maps known keys to PostMeta fields" $ do
      let parsed = parsePostMeta [("title", "t"), ("author", "a"), ("date", "d")]
      assertEq "parsePostMeta should decode all known fields"
        (PostMeta "t" "a" "d")
        parsed
  , mkTestCase "parsePostMeta fills missing keys with empty string" $ do
      let parsed = parsePostMeta [("title", "t")]
      assertEq "parsePostMeta should fallback missing fields to empty"
        (PostMeta "t" "" "")
        parsed
  , mkTestCase "splitFrontMatter parses metadata block between delimiters" $ do
      let content =
            unlines
              [ metaDelimiter
              , "title: Hello"
              , "author: Xilong"
              , "date: 2026-03-22"
              , metaDelimiter
              , ""
              , "Body"
              ]
      assertEq "splitFrontMatter should parse each metadata line"
        [("title", "Hello"), ("author", "Xilong"), ("date", "2026-03-22")]
        (splitFrontMatter "fixture.md" content)
  , mkTestCase "splitFrontMatter throws without opening delimiter" $
      assertThrows "splitFrontMatter should reject content without opening delimiter" $
        pure (splitFrontMatter "fixture.md" "title: x\n---\n")
  , mkTestCase "splitFrontMatter throws without closing delimiter" $
      assertThrows "splitFrontMatter should reject content without closing delimiter" $
        pure (splitFrontMatter "fixture.md" "title: ---\nk:v")
  , mkTestCase "parsePost loads content and resolves paths/url/meta" $ do
      let filename = "builder/UT/.mock/src/parse-post-fixture.md"
      let name = takeBaseName filename 
      let sourcePath = filename
      content <- readFile filename
      post <- parsePost filename
      assertEq "parsePost should derive postName from filename" (takeBaseName filename) (postName post)
      assertEq "parsePost should resolve source path" sourcePath (postSourcePath post)
      assertEq "parsePost should load full source markdown content" content (postContent post)
      assertEq "parsePost should parse front matter into PostMeta"
        (PostMeta "Fixture Title" "Fixture Author" "2026-03-22")
        (postMeta post)
  ]
