module UT.Modules.BuildJudger (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Modules.BuildJudger (shouldBuild)
import Modules.BuildPlan
import Modules.Post (Post(..), PostMeta(..))
import UT.TestUtils.Asserts
import UT.TestUtils.Paths
import UT.TestUtils.TestSuite

suiteName :: String
suiteName = "BuildJudger"

testCases :: [TestCase]
testCases =
  [ testShouldBuildIndexAlwaysTrue
  , testShouldBuildPostWhenTargetMissing
  , testShouldBuildPostWhenSourceNewer
  , testShouldNotBuildPostWhenTargetNewer
  ]

testShouldBuildIndexAlwaysTrue :: TestCase
testShouldBuildIndexAlwaysTrue =
  mkTestCase "shouldBuild always returns True for index plan" $ do
    let plan = mkBuildIndexPlan []
    result <- shouldBuild plan
    assertTrue "index plan should always rebuild" result

testShouldBuildPostWhenTargetMissing :: TestCase
testShouldBuildPostWhenTargetMissing =
  mkTestCase "shouldBuild returns True for post plan when target html is missing" $
    withCasePaths suiteName "postShouldBuildWhenTargetMissing" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-missing-source.md"
          target = postFile casePaths "build-plan-ut-missing-target.html"
      writeFile src "source"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- shouldBuild (BuildPostPlan plan)
      assertTrue "post should rebuild when target does not exist" result

testShouldBuildPostWhenSourceNewer :: TestCase
testShouldBuildPostWhenSourceNewer =
  mkTestCase "shouldBuild returns True for post plan when source is newer than target" $
    withCasePaths suiteName "postShouldBuildWhenSourceNewer" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-source-newer.md"
          target = postFile casePaths "build-plan-ut-source-newer.html"
      writeFile target "old-target"
      threadDelay 1100000
      writeFile src "new-source"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- shouldBuild (BuildPostPlan plan)
      assertTrue "post should rebuild when source is newer" result

testShouldNotBuildPostWhenTargetNewer :: TestCase
testShouldNotBuildPostWhenTargetNewer =
  mkTestCase "shouldBuild returns False for post plan when target is newer than source" $
    withCasePaths suiteName "postShouldNotBuildWhenTargetNewer" ["src", "post"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-target-newer.md"
          target = postFile casePaths "build-plan-ut-target-newer.html"
      writeFile src "old-source"
      threadDelay 1100000
      writeFile target "new-target"
      let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
      let plan = base { planTargetHtmlPath = target }
      result <- shouldBuild (BuildPostPlan plan)
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
