module Test.UT.Modules.TypeAlias (suiteName, testCases) where

import Modules.TypeAlias
import Test.Framework.Asserts
import Test.Framework.TestSuite

suiteName :: String
suiteName = "TypeAlias"

testCases :: [TestCase]
testCases =
  [ testTypeAliasValuesFlowAsString
  ]

testTypeAliasValuesFlowAsString :: TestCase
testTypeAliasValuesFlowAsString =
  mkTestCase "type aliases remain String-compatible at call sites" $ do
    let markdown :: Markdown
        markdown = "# Title"
        url :: Url
        url = "/post/a.html"
        html :: Html
        html = "<main></main>"
        json :: SearchJson
        json = "{\"posts\":[]}"
    assertEq "markdown alias should carry raw markdown text" "# Title" markdown
    assertEq "url alias should carry URL text" "/post/a.html" url
    assertEq "html alias should carry html text" "<main></main>" html
    assertEq "search json alias should carry json text" "{\"posts\":[]}" json
