module Test.UT.Modules.SearchDB (suiteName, testCases) where

import Modules.Post.Parse (parsePost)
import Modules.SearchDB
  ( SearchItem(..)
  , genSearchDB
  , mkSearchDB
  , mkSearchItem
  , mkSearchJson
  , postToIndexContentPair
  )
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

-- Suite for search database serialization and generation helpers.
suiteName :: String
suiteName = "SearchDB"

testCases :: [TestCase]
testCases =
  [ testMkSearchDBWrapsEntriesInPostsArray
  , testMkSearchJsonSerializesFields
  , testMkSearchItemMapsFromIndexPair
  , testPostToIndexContentPairNormalizesToSingleLine
  , testGenSearchDBWritesExpectedJsonFile
  ]

-- Confirms mkSearchDB wraps serialized entries in the expected top-level posts array.
testMkSearchDBWrapsEntriesInPostsArray :: TestCase
testMkSearchDBWrapsEntriesInPostsArray =
  mkTestCase "mkSearchDB wraps entries in a posts array object" $ do
    let rendered = mkSearchDB ["{\"title\":\"A\"}", "{\"title\":\"B\"}"]
    assertEq "mkSearchDB should concatenate entries with comma separator"
      "{\"posts\": [{\"title\":\"A\"}, {\"title\":\"B\"}]}"
      rendered

-- Confirms mkSearchJson serializes SearchItem into a flat JSON object string.
testMkSearchJsonSerializesFields :: TestCase
testMkSearchJsonSerializesFields =
  mkTestCase "mkSearchJson serializes SearchItem fields into flat JSON" $ do
    let item = SearchItem "Fixture Title" "/post/fixture.html" "some body"
    assertEq "mkSearchJson should render title/url/content fields"
      "{\"title\": \"Fixture Title\", \"url\": \"/post/fixture.html\", \"content\": \"some body\"}"
      (mkSearchJson item)

-- Confirms mkSearchItem copies title/url from IndexItem and keeps provided content.
testMkSearchItemMapsFromIndexPair :: TestCase
testMkSearchItemMapsFromIndexPair =
  mkTestCase "mkSearchItem maps tuple values from IndexItem and content" $
    withCasePathsInSandbox suiteName "mkSearchItemMapsFromIndexPair" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile parsePostFixturePath sourcePath
      withFakePandoc casePaths $ do
        parsed <- parsePost sourcePath
        post <- expectRight "parsePost should succeed for mkSearchItem fixture" parsed
        (item, _) <- postToIndexContentPair post
        let searchItem = mkSearchItem (item, "normalized content")
        assertEq "mkSearchItem should copy title"
          "Fixture Title"
          (searchItemTitle searchItem)
        assertEq "mkSearchItem currently keeps empty url placeholder until SearchDB url wiring lands"
          ""
          (searchItemUrl searchItem)
        assertEq "mkSearchItem should keep provided content"
          "normalized content"
          (searchItemContent searchItem)

-- Confirms postToIndexContentPair strips newlines and returns non-empty normalized content.
testPostToIndexContentPairNormalizesToSingleLine :: TestCase
testPostToIndexContentPairNormalizesToSingleLine =
  mkTestCase "postToIndexContentPair returns single-line normalized content" $
    withCasePathsInSandbox suiteName "postToIndexContentPairNormalizesToSingleLine" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile parsePostFixturePath sourcePath
      withFakePandoc casePaths $ do
        parsed <- parsePost sourcePath
        post <- expectRight "parsePost should succeed for postToIndexContentPair fixture" parsed
        (_, content) <- postToIndexContentPair post
        assertFalse "postToIndexContentPair should remove newline chars from content"
          ('\n' `elem` content)
        assertTrue "postToIndexContentPair should produce non-empty plain content"
          (not (null content))

-- Confirms genSearchDB writes JSON file containing expected posts/title/url fields.
testGenSearchDBWritesExpectedJsonFile :: TestCase
testGenSearchDBWritesExpectedJsonFile =
  mkTestCase "genSearchDB writes a JSON file containing post entry fields" $
    withCasePathsInSandbox suiteName "genSearchDBWritesExpectedJsonFile" ["src", "temp"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
          outputPath = tempFile casePaths "searchdb-ut-output.json"
      copyFile parsePostFixturePath sourcePath
      withFakePandoc casePaths $ do
        parsed <- parsePost sourcePath
        post <- expectRight "parsePost should succeed for genSearchDB fixture" parsed
        genSearchDB outputPath [post]
        exists <- doesFileExist outputPath
        assertTrue "genSearchDB should create output file" exists
        rendered <- readFile outputPath
        assertContains "searchdb should include top-level posts array key"
          "\"posts\": ["
          rendered
        assertContains "searchdb should include serialized title from post metadata"
          "\"title\": \"Fixture Title\""
          rendered
        assertContains "searchdb should include current empty-url placeholder emitted by SearchDB"
          "\"url\": \"\""
          rendered

expectRight :: String -> Either a b -> IO b
expectRight _ (Right value) = pure value
expectRight message (Left _) = error ("Assertion failed: " ++ message)

withFakePandoc :: CasePaths -> IO a -> IO a
withFakePandoc casePaths action = do
  let binDir = caseRootDir casePaths </> "bin"
      fakePandocPath = binDir </> "pandoc"
  createDirectoryIfMissing True binDir
  writeFile fakePandocPath $
    unlines
      [ "#!/bin/sh"
      , "printf '%s\\n' \"fixture plain content from fake pandoc\""
      ]
  callProcess "chmod" ["+x", fakePandocPath]
  withPrependedPath binDir action
