module Test.UT.Modules.BuildPlan (suiteName, testCases) where

import Modules.BuildPlan
import Modules.Config
import System.FilePath ((</>))
import Test.Framework.Asserts
import Test.Framework.Expect
import Test.Framework.Paths (srcFixtureFile)
import Test.Framework.TestSuite

suiteName :: String
suiteName = "BuildPlan"

testCases :: [TestCase]
testCases =
  [ testMkBuildPostPlanPaths
  , testMkBuildIndexPlanFields
  ]

-- Confirms post build plan derives expected output/template paths from source file.
testMkBuildPostPlanPaths :: TestCase
testMkBuildPostPlanPaths =
  mkTestCase "mkBuildPostPlan builds expected preprocess path, target path, and url" $ do
    let sourcePath = srcFixtureFile "hello-world.md"
    let plan = expectPostPlan (mkBuildPostPlan sourcePath)
    assertEq "mkBuildPostPlan should keep original source path" sourcePath (planPostSourcePath plan)
    assertEq "mkBuildPostPlan should generate temp preprocess markdown path"
      (tempPath </> "hello-world.md")
      (planPreprocessedPath plan)
    assertEq "mkBuildPostPlan should generate temp built html path"
      (tempPath </> "hello-world.html")
      (planBuiltHtmlPath plan)
    assertEq "mkBuildPostPlan should generate target html path"
      (postPath </> "hello-world.html")
      (planTargetHtmlPath plan)
    assertEq "mkBuildPostPlan should bind the rendered post template path"
      renderedTemplatePostPath
      (planPostTemplatePath plan)
    assertEq "mkBuildPostPlan should generate canonical post url"
      (webPostPath ++ "hello-world.html")
      (planPostUrl plan)

-- Confirms index build plan keeps canonical output/template/url fields.
testMkBuildIndexPlanFields :: TestCase
testMkBuildIndexPlanFields =
  mkTestCase "mkBuildIndexPlan stores canonical index output fields" $ do
    let plan = expectIndexPlan mkBuildIndexPlan
    assertEq "mkBuildIndexPlan should bind index html output path"
      indexPath
      (planIndexHtmlPath plan)
    assertEq "mkBuildIndexPlan should bind the rendered index template path"
      renderedTemplateIndexPath
      (planIndexTemplatePath plan)
    assertEq "mkBuildIndexPlan should generate canonical index url"
      (webRoot ++ "index.html")
      (planIndexUrl plan)
