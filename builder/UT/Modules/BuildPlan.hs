module UT.Modules.BuildPlan (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Modules.BuildPlan
import Modules.Config
import Modules.Post (Post(..), PostMeta(..))
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

-- Suite for build-plan construction and rebuild decision policy.
suiteName :: String
suiteName = "BuildPlan"

testCases :: [TestCase]
testCases =
  [ testMkBuildPostPlanPaths
  , testMkBuildIndexPlanFields
  , testShouldBuildIndexAlwaysTrue
  , testPostShouldBuildWhenTargetMissing
  , testPostShouldBuildWhenSourceNewer
  , testPostShouldNotBuildWhenTargetNewer
  ]

-- Confirms post build plan keeps post payload and derives expected output/template paths.
testMkBuildPostPlanPaths :: TestCase
testMkBuildPostPlanPaths =
  mkTestCase "mkBuildPostPlan builds expected preprocess path, target path, and url" $ do
    let post = mkPost "hello-world" "builder/UT/.fixture/src/hello-world.md"
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

-- Confirms shouldBuild forces rebuild for index plans.
testShouldBuildIndexAlwaysTrue :: TestCase
testShouldBuildIndexAlwaysTrue =
  mkTestCase "shouldBuild always returns True for index plan" $ do
    let plan = mkBuildIndexPlan []
    result <- shouldBuild plan
    assertTrue "index plan should always rebuild" result

-- Confirms post rebuild is required when target html file is absent.
testPostShouldBuildWhenTargetMissing :: TestCase
testPostShouldBuildWhenTargetMissing =
  mkTestCase "postShouldBuild returns True when target html is missing" $
    withCasePaths suiteName "postShouldBuildWhenTargetMissing" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-missing-source.md"
          target = postFile casePaths "build-plan-ut-missing-target.html"
      writeFile src "source"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- postShouldBuild plan
      assertTrue "post should rebuild when target does not exist" result

-- Confirms post rebuild is required when source timestamp is newer than target.
testPostShouldBuildWhenSourceNewer :: TestCase
testPostShouldBuildWhenSourceNewer =
  mkTestCase "postShouldBuild returns True when source is newer than target" $
    withCasePaths suiteName "postShouldBuildWhenSourceNewer" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-source-newer.md"
          target = postFile casePaths "build-plan-ut-source-newer.html"
      writeFile target "old-target"
      -- Keep a visible timestamp gap to avoid filesystem precision races.
      threadDelay 1100000
      writeFile src "new-source"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- postShouldBuild plan
      assertTrue "post should rebuild when source is newer" result

-- Confirms post rebuild is skipped when target timestamp is newer than source.
testPostShouldNotBuildWhenTargetNewer :: TestCase
testPostShouldNotBuildWhenTargetNewer =
  mkTestCase "postShouldBuild returns False when target is newer than source" $
    withCasePaths suiteName "postShouldNotBuildWhenTargetNewer" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-target-newer.md"
          target = postFile casePaths "build-plan-ut-target-newer.html"
      writeFile src "old-source"
      -- Keep a visible timestamp gap to avoid filesystem precision races.
      threadDelay 1100000
      writeFile target "new-target"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- postShouldBuild plan
      assertFalse "post should not rebuild when target is newer" result

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
