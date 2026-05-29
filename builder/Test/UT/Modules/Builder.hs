module Test.UT.Modules.Builder (suiteName, testCases) where

import Modules.BuildPlan
import Modules.Builder
import Modules.Index.Item (IndexItem(..))
import Modules.Post.Parse (parsePost)
import System.Directory
  ( copyFile
  , doesFileExist
  )
import Test.Framework.Asserts
import Test.Framework.Paths
import Test.Framework.TestSuite

-- Suite for builder integration-level execution helpers.
suiteName :: String
suiteName = "Builder"

testCases :: [TestCase]
testCases =
  [ testExecuteBuildPostPlan
  , testExecuteBuildIndexPlan
  ]

-- Confirms post build writes preprocess markdown and final html with expected transformations.
testExecuteBuildPostPlan :: TestCase
testExecuteBuildPostPlan =
  mkTestCase "executeBuildPlan runs post plan and writes html under .mock/post" $
    withCasePaths suiteName "executeBuildPostPlan" ["src", "post", "temp", "template"] $ \casePaths -> do
      let sourcePath = srcFile casePaths "parse-post-fixture.md"
          postTemplatePath = templateFile casePaths "post.html"
          outputPath = tempFile casePaths "builder-ut-preprocessed.md"
          builtHtmlPath = tempFile casePaths "builder-ut-built.html"
          htmlPath = postFile casePaths "builder-ut-output.html"
      copyFile parsePostFixturePath sourcePath
      copyFile postTemplateFixturePath postTemplatePath
      post <- parsePost sourcePath
      let basePlan = expectPostPlan (mkBuildPostPlan post)
      let plan =
            basePlan
              { planPreprocessedPath = outputPath
              , planBuiltHtmlPath = builtHtmlPath
              , planTargetHtmlPath = htmlPath
              , planPostTemplatePath = postTemplatePath
              }
      executeBuildPlan (BuildPostPlan plan)
      exists <- doesFileExist outputPath
      assertTrue "executeBuildPlan should create preprocess markdown file for post plan" exists
      written <- readFile outputPath
      assertContains "written preprocess file should include front matter title" "title: Fixture Title" written
      assertContains "written preprocess file should include toc marker" "[[toc]]" written
      assertContains "written preprocess file should include rewritten C language fence"
        "``` {.language-C .line-numbers .match-braces}"
        written
      htmlExists <- doesFileExist htmlPath
      assertTrue "executeBuildPlan should create rendered html output for post plan" htmlExists
      html <- readFile htmlPath
      assertContains "rendered html should include a heading generated from markdown content" "<h2 id=\"sub-title2\">" html
      assertContains "rendered html should include wrapped abstract block from source" "<div class=\"abstract\">" html
      assertContains "rendered html should preserve rewritten code block classes"
        "<pre class=\"language-C line-numbers match-braces\">"
        html

-- Confirms index build renders posts into template and writes target html file.
testExecuteBuildIndexPlan :: TestCase
testExecuteBuildIndexPlan =
  mkTestCase "executeBuildPlan writes replaced index html to target file" $
    withCasePaths suiteName "executeBuildIndexPlan" ["post", "template"] $ \casePaths -> do
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
      executeBuildPlan (BuildIndexPlan plan)
      exists <- doesFileExist indexOutputPath
      assertTrue "executeBuildPlan should create index output html file for index plan" exists
      html <- readFile indexOutputPath
      assertContains "index output should include year heading from items" "<h3>2026</h3>" html
      assertContains "index output should include item link title" "Index Title" html
      assertContains "index output should include item link url" "/post/index-title.html" html

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"
