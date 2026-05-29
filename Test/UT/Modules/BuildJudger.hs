module Test.UT.Modules.BuildJudger (suiteName, testCases) where

import Modules.BuildJudger (shouldBuild)
import Modules.BuildPlan
import Modules.Config
import Modules.Utils.Files (hashUpdate)
import Control.Concurrent (threadDelay)
import System.FilePath ((</>))
import Test.Framework.Asserts
import Test.Framework.Expect
import Test.Framework.Paths
import Test.Framework.TestSuite

suiteName :: String
suiteName = "BuildJudger"

testCases :: [TestCase]
testCases =
  [ testShouldBuildIndexWhenMetaStateMissing
  , testShouldBuildIndexWhenTargetMissing
  , testShouldNotBuildIndexWhenBuilderAndMetaUnchanged
  , testShouldBuildIndexWhenBuilderHashChanged
  , testShouldBuildPostWhenTargetMissing
  , testShouldNotBuildPostWhenTargetAndStateAreCurrent
  , testShouldBuildPostWhenSourceNewerThanTarget
  , testShouldBuildPostWhenSourceHashChanged
  ]

testShouldBuildIndexWhenMetaStateMissing :: TestCase
testShouldBuildIndexWhenMetaStateMissing =
  mkTestCase "shouldBuild returns True for index plan when metadata state file is missing" $
    withCasePathsInSandbox suiteName "indexShouldBuildWhenMetaStateMissing" ["Src", ".cache/artifacts/meta"] $ \_ -> do
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      writeFile (metaArtifactsPath </> "item.klb") "size:1\nx:y\n"
      hashUpdate builderPath builderStatePath
      result <- shouldBuild mkBuildIndexPlan
      assertTrue "index plan should rebuild when meta-state cache does not exist yet" result

testShouldBuildIndexWhenTargetMissing :: TestCase
testShouldBuildIndexWhenTargetMissing =
  mkTestCase "shouldBuild returns True for index plan when index target file is missing" $
    withCasePathsInSandbox suiteName "indexShouldBuildWhenTargetMissing" ["Src", ".cache/artifacts/meta"] $ \_ -> do
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      writeFile (metaArtifactsPath </> "item.klb") "size:1\nx:y\n"
      hashUpdate builderPath builderStatePath
      hashUpdate metaArtifactsPath metaStatePath
      result <- shouldBuild mkBuildIndexPlan
      assertTrue "index plan should rebuild when index.html target does not exist" result

testShouldNotBuildIndexWhenBuilderAndMetaUnchanged :: TestCase
testShouldNotBuildIndexWhenBuilderAndMetaUnchanged =
  mkTestCase "shouldBuild returns False for index plan when builder and metadata hashes are unchanged" $
    withCasePathsInSandbox suiteName "indexShouldNotBuildWhenUnchanged" ["Src", ".cache/artifacts/meta"] $ \_ -> do
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      writeFile (metaArtifactsPath </> "item.klb") "size:1\nx:y\n"
      writeFile indexPath "<!doctype html>"
      hashUpdate builderPath builderStatePath
      hashUpdate metaArtifactsPath metaStatePath
      result <- shouldBuild mkBuildIndexPlan
      assertFalse "index plan should skip rebuild when builder and meta artifacts are unchanged" result

testShouldBuildIndexWhenBuilderHashChanged :: TestCase
testShouldBuildIndexWhenBuilderHashChanged =
  mkTestCase "shouldBuild returns True for index plan when builder source hash changes" $
    withCasePathsInSandbox suiteName "indexShouldBuildWhenBuilderHashChanged" ["Src", ".cache/artifacts/meta"] $ \_ -> do
      let builderStub = builderPath </> "build-judger-ut-builder-stub.hs"
      writeFile builderStub "v1"
      writeFile (metaArtifactsPath </> "item.klb") "size:1\nx:y\n"
      writeFile indexPath "<!doctype html>"
      hashUpdate builderPath builderStatePath
      hashUpdate metaArtifactsPath metaStatePath
      writeFile builderStub "v2"
      result <- shouldBuild mkBuildIndexPlan
      assertTrue "index plan should rebuild when builder hash differs from cached state" result

testShouldBuildPostWhenTargetMissing :: TestCase
testShouldBuildPostWhenTargetMissing =
  mkTestCase "shouldBuild returns True for post plan when target html is missing" $
    withCasePathsInSandbox suiteName "postShouldBuildWhenTargetMissing" ["src", "post", "Src"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-missing-source.md"
          target = postFile casePaths "build-plan-ut-missing-target.html"
      writeFile src "source"
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      hashUpdate builderPath builderStatePath
      let base = expectPostPlan (mkBuildPostPlan src)
      let plan = base { planTargetHtmlPath = target }
      hashUpdate src (planPostStatePath plan)
      result <- shouldBuild (BuildPostPlan plan)
      assertTrue "post should rebuild when target does not exist" result

testShouldNotBuildPostWhenTargetAndStateAreCurrent :: TestCase
testShouldNotBuildPostWhenTargetAndStateAreCurrent =
  mkTestCase "shouldBuild returns False for post plan when target exists and source hash is unchanged" $
    withCasePathsInSandbox suiteName "postShouldNotBuildWhenTargetAndStateCurrent" ["src", "post", "Src"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-source-current.md"
          target = postFile casePaths "build-plan-ut-target-current.html"
      writeFile src "same-source"
      writeFile target "existing-target"
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      hashUpdate builderPath builderStatePath
      let base = expectPostPlan (mkBuildPostPlan src)
      let plan = base { planTargetHtmlPath = target }
      hashUpdate src (planPostStatePath plan)
      result <- shouldBuild (BuildPostPlan plan)
      assertFalse "post should skip rebuild when builder hash, target and source state are all unchanged" result

testShouldBuildPostWhenSourceNewerThanTarget :: TestCase
testShouldBuildPostWhenSourceNewerThanTarget =
  mkTestCase "shouldBuild returns True for post plan when source mtime is newer than target mtime" $
    withCasePathsInSandbox suiteName "postShouldBuildWhenSourceNewer" ["src", "post", "Src"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-source-newer.md"
          target = postFile casePaths "build-plan-ut-source-newer.html"
      writeFile target "existing-target"
      threadDelay 1100000
      writeFile src "same-source"
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      hashUpdate builderPath builderStatePath
      let base = expectPostPlan (mkBuildPostPlan src)
      let plan = base { planTargetHtmlPath = target }
      hashUpdate src (planPostStatePath plan)
      result <- shouldBuild (BuildPostPlan plan)
      assertTrue "post should rebuild when source file is newer than target html" result

testShouldBuildPostWhenSourceHashChanged :: TestCase
testShouldBuildPostWhenSourceHashChanged =
  mkTestCase "shouldBuild returns True for post plan when source hash differs from cached state" $
    withCasePathsInSandbox suiteName "postShouldBuildWhenSourceHashChanged" ["src", "post", "Src"] $ \casePaths -> do
      let src = srcFile casePaths "build-plan-ut-source-changed.md"
          target = postFile casePaths "build-plan-ut-target-existing.html"
      writeFile src "old-source"
      writeFile target "existing-target"
      writeFile (builderPath </> "build-judger-ut-builder-stub.hs") "stub"
      hashUpdate builderPath builderStatePath
      let base = expectPostPlan (mkBuildPostPlan src)
      let plan = base { planTargetHtmlPath = target }
      hashUpdate src (planPostStatePath plan)
      writeFile src "new-source"
      result <- shouldBuild (BuildPostPlan plan)
      assertTrue "post should rebuild when source hash changes after state snapshot" result
