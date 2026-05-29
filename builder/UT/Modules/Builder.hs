module UT.Modules.Builder (suiteName, testCases) where

import Modules.BuildPlan
import Modules.Builder
import Modules.IndexItem (IndexItem(..))
import Modules.Post (parsePost)
import System.Directory
  ( copyFile
  , doesFileExist
  )
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

fixtureSourcePath :: FilePath
fixtureSourcePath = "builder/UT/.fixture/src/parse-post-fixture.md"

fixturePostTemplatePath :: FilePath
fixturePostTemplatePath = "builder/UT/.fixture/template/post.html"

-- Suite for builder integration-level execution helpers.
suiteName :: String
suiteName = "Builder"

testCases :: [TestCase]
testCases =
  [ testBuildPostWithPlan
  , testBuildIndexWithPlan
  ]

-- Confirms post build writes preprocess markdown and final html with expected transformations.
testBuildPostWithPlan :: TestCase
testBuildPostWithPlan =
  mkTestCase "buildPostWithPlan runs real pandoc and writes html under .mock/post" $
    withCasePaths suiteName "buildPostWithPlan" ["src", "post", "temp", "template"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
          postTemplatePath = templateFile casePaths "post.html"
          outputPath = tempFile casePaths "builder-ut-preprocessed.md"
          builtHtmlPath = tempFile casePaths "builder-ut-built.html"
          htmlPath = postFile casePaths "builder-ut-output.html"
      copyFile fixtureSourcePath sourcePath
      copyFile fixturePostTemplatePath postTemplatePath
      post <- parsePost sourcePath
      let basePlan = expectPostPlan (mkBuildPostPlan post)
      let plan =
            basePlan
              { planPreprocessedPath = outputPath
              , planBuiltHtmlPath = builtHtmlPath
              , planTargetHtmlPath = htmlPath
              , planPostTemplatePath = postTemplatePath
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

-- Confirms index build renders posts into template and writes target html file.
testBuildIndexWithPlan :: TestCase
testBuildIndexWithPlan =
  mkTestCase "buildIndexWithPlan writes replaced index html to target file" $
    withCasePaths suiteName "buildIndexWithPlan" ["post", "template"] $ \casePaths -> do
      let indexTemplatePath = templateFile casePaths "index.html"
          indexOutputPath = postFile casePaths "builder-ut-index.html"
      writeFile indexTemplatePath "<html><body>$posts$</body></html>"
      let plan =
            IndexBuildPlan
              { planIndexItems =
                  [IndexItem "Index Title" "2026" "03" "22" "/post/index-title.html"]
              , planIndexHtmlPath = indexOutputPath
              , planIndexTemplatePath = indexTemplatePath
              , planIndexUrl = "/index.html"
              }
      buildIndexWithPlan plan
      exists <- doesFileExist indexOutputPath
      assertTrue "buildIndexWithPlan should create index output html file" exists
      html <- readFile indexOutputPath
      assertContains "index output should include year heading from items" "<h3>2026</h3>" html
      assertContains "index output should include item link title" "Index Title" html
      assertContains "index output should include item link url" "/post/index-title.html" html

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"
