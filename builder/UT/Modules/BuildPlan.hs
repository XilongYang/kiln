module UT.Modules.BuildPlan (suiteName, testCases) where

import Modules.BuildPlan
import Modules.Config
import Modules.Post (Post(..), PostMeta(..))
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.Paths (srcFixtureFile)
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "BuildPlan"

testCases :: [TestCase]
testCases =
  [ testMkBuildPostPlanPaths
  , testMkBuildIndexPlanFields
  ]

-- Confirms post build plan keeps post payload and derives expected output/template paths.
testMkBuildPostPlanPaths :: TestCase
testMkBuildPostPlanPaths =
  mkTestCase "mkBuildPostPlan builds expected preprocess path, target path, and url" $ do
    let post = mkPost "hello-world" (srcFixtureFile "hello-world.md")
    let plan = expectPostPlan (mkBuildPostPlan post)
    assertEq "mkBuildPostPlan should keep original post payload" post (planPost plan)
    assertEq "mkBuildPostPlan should generate temp preprocess markdown path"
      (tempPath </> "hello-world.md")
      (planPreprocessedPath plan)
    assertEq "mkBuildPostPlan should generate target html path"
      (postPath </> "hello-world.html")
      (planTargetHtmlPath plan)
    assertEq "mkBuildPostPlan should bind the rendered post template path"
      renderedTemplatePostPath
      (planPostTemplatePath plan)

-- Confirms index build plan stores items and canonical template/url fields.
testMkBuildIndexPlanFields :: TestCase
testMkBuildIndexPlanFields =
  mkTestCase "mkBuildIndexPlan stores posts and index url" $ do
    let posts = [mkPost "a" "a.md", mkPost "b" "b.md"]
    let plan = expectIndexPlan (mkBuildIndexPlan posts)
    assertEq "mkBuildIndexPlan should create one index item per post"
      (length posts)
      (length (planIndexItems plan))
    assertEq "mkBuildIndexPlan should bind the rendered index template path"
      renderedTemplateIndexPath
      (planIndexTemplatePath plan)
    assertEq "mkBuildIndexPlan should generate canonical index url"
      (webRoot ++ "index.html")
      (planIndexUrl plan)

mkPost :: String -> FilePath -> Post
mkPost name sourcePath =
  Post
    { postName = name
    , postSourcePath = sourcePath
    , postBody = ""
    , postAbstract = ""
    , postMeta = PostMeta "" "" ""
    }

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"

expectIndexPlan :: BuildPlan -> IndexBuildPlan
expectIndexPlan (BuildIndexPlan plan) = plan
expectIndexPlan _ = error "expected BuildIndexPlan"
