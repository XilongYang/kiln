module UT.Modules.Post (suiteName, testCases) where

import Modules.Post
  ( Post(..)
  , PostMeta(..)
  , extractPostAbstract
  , metaDelimiter
  , parseMetaLine
  , parsePost
  , parsePostMeta
  , revertMeta
  , splitFrontMatter
  )

import System.FilePath (takeBaseName)
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

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
  , mkTestCase "revertMeta writes canonical markdown front matter text" $ do
      let rendered = revertMeta (PostMeta "T" "A" "2026-03-22")
      assertEq "revertMeta should serialize delimiters and keys in fixed order"
        (unlines
          [ metaDelimiter
          , "title: T"
          , "author: A"
          , "date: 2026-03-22"
          , metaDelimiter
          ])
        rendered
  , mkTestCase "revertMeta round-trip keeps PostMeta fields unchanged" $ do
      let meta = PostMeta "Fixture Title" "Fixture Author" "2026-03-22"
      let serialized = revertMeta meta
      let (pairs, _) = splitFrontMatter "roundtrip.md" (serialized ++ "\nbody")
      assertEq "parsePostMeta . splitFrontMatter . revertMeta should preserve metadata"
        meta
        (parsePostMeta pairs)
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
      let (meta, remain) = splitFrontMatter "fixture.md" content
      assertEq "splitFrontMatter should parse each metadata line"
        [("title", "Hello"), ("author", "Xilong"), ("date", "2026-03-22")]
        meta
      assertEq "splitFrontMatter should keep remaining body payload"
        "\nBody\n"
        remain
  , mkTestCase "extractPostAbstract splits around section marker token" $ do
      let content = "intro line\n## \ncontent line\n"
      let (abstract, remain) = extractPostAbstract content
      assertEq "extractPostAbstract should return intro block before marker"
        "intro line\n"
        abstract
      assertEq "extractPostAbstract should return marker and trailing lines as content"
        "## \ncontent line\n"
        remain
  , mkTestCase "splitFrontMatter throws without opening delimiter" $
      assertThrows "splitFrontMatter should reject content without opening delimiter" $
        pure (splitFrontMatter "fixture.md" "title: x\n---\n")
  , mkTestCase "splitFrontMatter throws without closing delimiter" $
      assertThrows "splitFrontMatter should reject content without closing delimiter" $
        pure (splitFrontMatter "fixture.md" "title: ---\nk:v")
  , mkTestCase "parsePost loads content and resolves paths/url/meta" $ do
      let sourcePath = "builder/UT/.mock/src/parse-post-fixture.md"
      post <- parsePost sourcePath
      assertEq "parsePost should derive postName from filename" (takeBaseName sourcePath) (postName post)
      assertEq "parsePost should resolve source path" sourcePath (postSourcePath post)
      assertEq "parsePost should keep extracted abstract block"
        "\nAbstract l##ine1\n### Abstract line2##\n##Abstract line3\n\n"
        (postAbstract post)
      assertEq "parsePost should keep remaining markdown body"
        "## Sub Title1\n\nbodys\nbodys\nbodys\n\n```C\nCode1\n```\n\n## Sub Title2\n\nbodys\nbodys\nbodys\n\n```Haskell\nCode2\n```\n"
        (postContent post)
      assertEq "parsePost should parse front matter into PostMeta"
        (PostMeta "Fixture Title" "Fixture Author" "2026-03-22")
        (postMeta post)
  ]
