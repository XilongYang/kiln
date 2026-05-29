module Test.UT.Modules.Post.Parse (suiteName, testCases) where

import Modules.Post (PostMeta(..), postMeta, postAbstract, postBody)
import Modules.Post.Parse (parsePost)
import System.Directory (copyFile)
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Post.Parse"

testCases :: [TestCase]
testCases =
  [ testParsePostLoadsAndResolvesFields
  , testParsePostReturnsLeftWithoutOpeningDelimiter
  , testParsePostReturnsLeftWithoutClosingDelimiter
  , testParsePostReturnsLeftWhenRequiredMetaMissing
  ]

testParsePostLoadsAndResolvesFields :: TestCase
testParsePostLoadsAndResolvesFields =
  mkTestCase "parsePost loads content and resolves meta/body fields" $
    withCasePathsInSandbox suiteName "parsePostLoadsAndResolvesFields" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile parsePostFixturePath sourcePath
      result <- parsePost sourcePath
      post <- expectRight "parsePost should successfully parse valid fixture" result
      assertEq "parsePost should parse front matter into PostMeta"
        (PostMeta "Fixture Title" "Fixture Author" "2026-03-22")
        (postMeta post)
      assertTrue "parsePost should produce non-empty abstract" (not (null (postAbstract post)))
      assertContains "parsePost should keep body section after abstract split"
        "## Sub Title1"
        (postBody post)

testParsePostReturnsLeftWithoutOpeningDelimiter :: TestCase
testParsePostReturnsLeftWithoutOpeningDelimiter =
  mkTestCase "parsePost returns Left without opening delimiter" $
    withCasePathsInSandbox suiteName "parsePostReturnsLeftWithoutOpeningDelimiter" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-opening.md"
      writeFile sourcePath "title: x\n---\nbody\n"
      result <- parsePost sourcePath
      assertTrue "parsePost should reject content without opening delimiter" (isLeft result)

testParsePostReturnsLeftWithoutClosingDelimiter :: TestCase
testParsePostReturnsLeftWithoutClosingDelimiter =
  mkTestCase "parsePost returns Left without closing delimiter" $
    withCasePathsInSandbox suiteName "parsePostReturnsLeftWithoutClosingDelimiter" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-closing.md"
      writeFile sourcePath (unlines ["---", "title: X", "author: Y", "date: 2026-03-22", "body"])
      result <- parsePost sourcePath
      assertTrue "parsePost should reject content without closing delimiter" (isLeft result)

testParsePostReturnsLeftWhenRequiredMetaMissing :: TestCase
testParsePostReturnsLeftWhenRequiredMetaMissing =
  mkTestCase "parsePost returns Left when required meta keys are missing" $
    withCasePathsInSandbox suiteName "parsePostReturnsLeftWhenRequiredMetaMissing" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "missing-meta.md"
      writeFile sourcePath (unlines ["---", "title: X", "date: 2026-03-22", "---", "", "Body"])
      result <- parsePost sourcePath
      assertTrue "parsePost should fail when required metadata keys are missing" (isLeft result)

expectRight :: String -> Either a b -> IO b
expectRight _ (Right value) = pure value
expectRight message (Left _) = error ("Assertion failed: " ++ message)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
