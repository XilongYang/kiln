module UT.Modules.SearchDB (suiteName, testCases) where

import Modules.Post (parsePost)
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
  , doesFileExist
  )
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

-- Suite for search database serialization and generation helpers.
suiteName :: String
suiteName = "SearchDB"

fixtureSourcePath :: FilePath
fixtureSourcePath = "builder/UT/.fixture/src/parse-post-fixture.md"

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
    withCasePaths suiteName "mkSearchItemMapsFromIndexPair" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile fixtureSourcePath sourcePath
      post <- parsePost sourcePath
      (item, _) <- postToIndexContentPair post
      let searchItem = mkSearchItem (item, "normalized content")
      assertEq "mkSearchItem should copy title"
        "Fixture Title"
        (searchItemTitle searchItem)
      assertEq "mkSearchItem should copy url"
        "/post/parse-post-fixture.html"
        (searchItemUrl searchItem)
      assertEq "mkSearchItem should keep provided content"
        "normalized content"
        (searchItemContent searchItem)

-- Confirms postToIndexContentPair strips newlines and returns non-empty normalized content.
testPostToIndexContentPairNormalizesToSingleLine :: TestCase
testPostToIndexContentPairNormalizesToSingleLine =
  mkTestCase "postToIndexContentPair returns single-line normalized content" $
    withCasePaths suiteName "postToIndexContentPairNormalizesToSingleLine" ["src"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
      copyFile fixtureSourcePath sourcePath
      post <- parsePost sourcePath
      (_, content) <- postToIndexContentPair post
      assertFalse "postToIndexContentPair should remove newline chars from content"
        ('\n' `elem` content)
      assertTrue "postToIndexContentPair should produce non-empty plain content"
        (not (null content))

-- Confirms genSearchDB writes JSON file containing expected posts/title/url fields.
testGenSearchDBWritesExpectedJsonFile :: TestCase
testGenSearchDBWritesExpectedJsonFile =
  mkTestCase "genSearchDB writes a JSON file containing post entry fields" $
    withCasePaths suiteName "genSearchDBWritesExpectedJsonFile" ["src", "temp"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
          outputPath = tempFile casePaths "searchdb-ut-output.json"
      copyFile fixtureSourcePath sourcePath
      post <- parsePost sourcePath
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
      assertContains "searchdb should include generated post url"
        "\"url\": \"/post/parse-post-fixture.html\""
        rendered
