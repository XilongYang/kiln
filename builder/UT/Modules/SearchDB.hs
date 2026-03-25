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
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

-- Suite for search database serialization and generation helpers.
suiteName :: String
suiteName = "SearchDB"

testCases :: [TestCase]
testCases =
  [ mkTestCase "mkSearchDB wraps entries in a posts array object" $ do
      let rendered = mkSearchDB ["{\"title\":\"A\"}", "{\"title\":\"B\"}"]
      assertEq "mkSearchDB should concatenate entries with comma separator"
        "{\"posts\": [{\"title\":\"A\"}, {\"title\":\"B\"}]}"
        rendered
  , mkTestCase "mkSearchJson serializes SearchItem fields into flat JSON" $ do
      let item = SearchItem "Fixture Title" "/post/fixture.html" "some body"
      assertEq "mkSearchJson should render title/url/content fields"
        "{\"title\": \"Fixture Title\", \"url\": \"/post/fixture.html\", \"content\": \"some body\"}"
        (mkSearchJson item)
  , mkTestCase "mkSearchItem maps tuple values from IndexItem and content" $ do
      post <- parsePost "builder/UT/.mock/src/parse-post-fixture.md"
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
  , mkTestCase "postToIndexContentPair returns single-line normalized content" $ do
      post <- parsePost "builder/UT/.mock/src/parse-post-fixture.md"
      (_, content) <- postToIndexContentPair post
      assertFalse "postToIndexContentPair should remove newline chars from content"
        ('\n' `elem` content)
      assertTrue "postToIndexContentPair should produce non-empty plain content"
        (not (null content))
  , mkTestCase "genSearchDB writes a JSON file containing post entry fields" $ do
      createDirectoryIfMissing True mockTempDir
      post <- parsePost "builder/UT/.mock/src/parse-post-fixture.md"
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
  ]
  where
    mockTempDir = "builder/UT/.mock"
    outputPath = mockTempDir </> "searchdb-ut-output.json"
