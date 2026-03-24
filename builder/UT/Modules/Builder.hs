module UT.Modules.Builder (suiteName, testCases) where

import Control.Monad (when)
import Modules.BuildPlan
import Modules.Builder
import Modules.IndexItem (IndexItem(..))
import Modules.Post (parsePost)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , removeFile
  )
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

mockTempDir :: FilePath
mockTempDir = "builder/UT/.mock/temp"

mockPostDir :: FilePath
mockPostDir = "builder/UT/.mock/post"

mockPostTemplatePath :: FilePath
mockPostTemplatePath = mockTempDir </> "post.html"

mockIndexTemplatePath :: FilePath
mockIndexTemplatePath = mockTempDir </> "index.html"

mockIndexOutputPath :: FilePath
mockIndexOutputPath = mockPostDir </> "builder-ut-index.html"

-- Suite for builder integration-level execution helpers.
suiteName :: String
suiteName = "Builder"

testCases :: [TestCase]
testCases =
  [ mkTestCase "buildPostWithPlan runs real pandoc and writes html under .mock/post" $
      withFreshMockFile outputPath $
      withFreshMockFile builtHtmlPath $
      withFreshMockFile htmlPath $ do
        createDirectoryIfMissing True mockTempDir
        createDirectoryIfMissing True mockPostDir
        post <- parsePost "builder/UT/.mock/src/parse-post-fixture.md"
        let basePlan = expectPostPlan (mkBuildPostPlan post)
        let plan =
              basePlan
                { planPreprocessedPath = outputPath
                , planBuiltHtmlPath = builtHtmlPath
                , planTargetHtmlPath = htmlPath
                , planPostTemplatePath = mockPostTemplatePath
                }
        buildPostWithPlan plan
        exists <- doesFileExist outputPath
        assertTrue "buildPostWithPlan should create preprocess markdown file" exists
        written <- readFile outputPath
        assertContains "written preprocess file should include front matter title" "title: Fixture Title" written
        assertContains "written preprocess file should include toc marker" "[[toc]]" written
        assertContains "written preprocess file should include rewritten C language fence"
          "``` {.language-C .line-numbers .match-braces}"
          written
        htmlExists <- doesFileExist htmlPath
        assertTrue "buildPostWithPlan should create rendered html output" htmlExists
        html <- readFile htmlPath
        assertContains "rendered html should include a heading generated from markdown content" "<h2 id=\"sub-title2\">" html
        assertContains "rendered html should include wrapped abstract block from source" "<div class=\"abstract\">" html
        assertContains "rendered html should preserve rewritten code block classes"
          "<pre class=\"language-C line-numbers match-braces\">"
          html
  , mkTestCase "buildIndexWithPlan writes replaced index html to target file" $
      withFreshMockFile mockIndexTemplatePath $
      withFreshMockFile mockIndexOutputPath $ do
        createDirectoryIfMissing True mockTempDir
        createDirectoryIfMissing True mockPostDir
        writeFile mockIndexTemplatePath "<html><body>$posts$</body></html>"
        let plan =
              IndexBuildPlan
                { planIndexItems =
                    [IndexItem "Index Title" "2026" "03" "22" "/post/index-title.html"]
                , planIndexHtmlPath = mockIndexOutputPath
                , planIndexTemplatePath = mockIndexTemplatePath
                , planIndexUrl = "/index.html"
                }
        buildIndexWithPlan plan
        exists <- doesFileExist mockIndexOutputPath
        assertTrue "buildIndexWithPlan should create index output html file" exists
        html <- readFile mockIndexOutputPath
        assertContains "index output should include year heading from items" "<h3>2026</h3>" html
        assertContains "index output should include item link title" "Index Title" html
        assertContains "index output should include item link url" "/post/index-title.html" html
  ]
  where
    outputPath = mockTempDir </> "builder-ut-preprocessed.md"
    builtHtmlPath = mockTempDir </> "builder-ut-built.html"
    htmlPath = mockPostDir </> "builder-ut-output.html"

withFreshMockFile :: FilePath -> IO a -> IO a
withFreshMockFile path action = do
  exists <- doesFileExist path
  when exists (removeFile path)
  action

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"
