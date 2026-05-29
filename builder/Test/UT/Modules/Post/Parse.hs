module Test.UT.Modules.Post.Parse (suiteName, testCases) where

import Modules.Post (Post(..), PostMeta(..))
import Modules.Post.Parse (parsePost)
import System.Directory (copyFile)
import System.FilePath (takeBaseName)
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Post.Parse"

testCases :: [TestCase]
testCases =
  [ testParsePostLoadsAndResolvesFields
  , testParsePostThrowsWithoutOpeningDelimiter
  , testParsePostThrowsWithoutClosingDelimiter
  , testParsePostThrowsWhenRequiredMetaMissing
  ]

testParsePostLoadsAndResolvesFields :: TestCase
testParsePostLoadsAndResolvesFields =
  mkTestCase "parsePost loads content and resolves paths/meta fields" $
    withCasePaths suiteName "parsePostLoadsAndResolvesFields" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile parsePostFixturePath sourcePath
      post <- parsePost sourcePath
      assertEq "parsePost should derive postName from filename" (takeBaseName sourcePath) (postName post)
      assertEq "parsePost should resolve source path" sourcePath (postSourcePath post)
      assertEq "parsePost should parse front matter into PostMeta"
        (PostMeta "Fixture Title" "Fixture Author" "2026-03-22")
        (postMeta post)
      assertTrue "parsePost should produce non-empty abstract" (not (null (postAbstract post)))
      assertContains "parsePost should keep body section after abstract split"
        "## Sub Title1"
        (postBody post)

testParsePostThrowsWithoutOpeningDelimiter :: TestCase
testParsePostThrowsWithoutOpeningDelimiter =
  mkTestCase "parsePost throws without opening delimiter" $
    withCasePaths suiteName "parsePostThrowsWithoutOpeningDelimiter" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-opening.md"
      writeFile sourcePath "title: x\n---\nbody\n"
      assertThrows "parsePost should reject content without opening delimiter" $
        forceParsedPost sourcePath

testParsePostThrowsWithoutClosingDelimiter :: TestCase
testParsePostThrowsWithoutClosingDelimiter =
  mkTestCase "parsePost throws without closing delimiter" $
    withCasePaths suiteName "parsePostThrowsWithoutClosingDelimiter" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-closing.md"
      writeFile sourcePath (unlines ["---", "title: X", "author: Y", "date: 2026-03-22", "body"])
      assertThrows "parsePost should reject content without closing delimiter" $
        forceParsedPost sourcePath

testParsePostThrowsWhenRequiredMetaMissing :: TestCase
testParsePostThrowsWhenRequiredMetaMissing =
  mkTestCase "parsePost throws when required meta keys are missing" $
    withCasePaths suiteName "parsePostThrowsWhenRequiredMetaMissing" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-meta.md"
      writeFile sourcePath (unlines ["---", "title: X", "date: 2026-03-22", "---", "", "Body"])
      assertThrows "parsePost should fail when required metadata keys are missing" $
        forceParsedPost sourcePath

forceParsedPost :: FilePath -> IO Int
forceParsedPost sourcePath = do
  post <- parsePost sourcePath
  let meta = postMeta post
  pure $! length (postBody post) + length (metaTitle meta) + length (metaAuthor meta) + length (metaDate meta)
