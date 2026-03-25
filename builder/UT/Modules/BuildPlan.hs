module UT.Modules.BuildPlan (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Modules.BuildPlan
import Modules.Config
import Modules.Post (Post(..), PostMeta(..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , removeFile
  )
import System.FilePath ((</>))
import UT.TestUtils.Asserts
import UT.TestUtils.TestSuite

mockSrcDir :: FilePath
mockSrcDir = "builder/UT/.mock/src"

mockPostDir :: FilePath
mockPostDir = "builder/UT/.mock/post"

-- Suite for build-plan construction and rebuild decision policy.
suiteName :: String
suiteName = "BuildPlan"

testCases :: [TestCase]
testCases =
  [ mkTestCase "mkBuildPostPlan builds expected preprocess path, target path, and url" $ do
      let post = mkPost "hello-world" "builder/UT/.mock/src/hello-world.md"
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
  , mkTestCase "mkBuildIndexPlan stores posts and index url" $ do
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
  , mkTestCase "shouldBuild always returns True for index plan" $ do
      let plan = mkBuildIndexPlan []
      result <- shouldBuild plan
      assertTrue "index plan should always rebuild" result
  , mkTestCase "postShouldBuild returns True when target html is missing" $
      let src = mockSrcDir </> "build-plan-ut-missing-source.md"
          target = mockPostDir </> "build-plan-ut-missing-target.html"
      in withFreshMockFiles [src, target] $ do
          writeFile src "source"
          let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
          let plan = base { planTargetHtmlPath = target }
          result <- postShouldBuild plan
          assertTrue "post should rebuild when target does not exist" result
  , mkTestCase "postShouldBuild returns True when source is newer than target" $
      let src = mockSrcDir </> "build-plan-ut-source-newer.md"
          target = mockPostDir </> "build-plan-ut-source-newer.html"
      in withFreshMockFiles [src, target] $ do
          writeFile target "old-target"
          -- Keep a visible timestamp gap to avoid filesystem precision races.
          threadDelay 1100000
          writeFile src "new-source"
          let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
          let plan = base { planTargetHtmlPath = target }
          result <- postShouldBuild plan
          assertTrue "post should rebuild when source is newer" result
  , mkTestCase "postShouldBuild returns False when target is newer than source" $
      let src = mockSrcDir </> "build-plan-ut-target-newer.md"
          target = mockPostDir </> "build-plan-ut-target-newer.html"
      in withFreshMockFiles [src, target] $ do
          writeFile src "old-source"
          -- Keep a visible timestamp gap to avoid filesystem precision races.
          threadDelay 1100000
          writeFile target "new-target"
          let base = expectPostPlan (mkBuildPostPlan (mkPost "x" src))
          let plan = base { planTargetHtmlPath = target }
          result <- postShouldBuild plan
          assertFalse "post should not rebuild when target is newer" result
  ]

mkPost :: String -> FilePath -> Post
mkPost name sourcePath =
  Post
    { postName = name
    , postSourcePath = sourcePath
    , postBody = ""
    , postAbstract = ""
    , postMeta = PostMeta "" "" ""
    }

withFreshMockFiles :: [FilePath] -> IO a -> IO a
withFreshMockFiles files action = do
  createDirectoryIfMissing True mockSrcDir
  createDirectoryIfMissing True mockPostDir
  mapM_ removeIfExists files
  action

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

expectPostPlan :: BuildPlan -> PostBuildPlan
expectPostPlan (BuildPostPlan plan) = plan
expectPostPlan _ = error "expected BuildPostPlan"

expectIndexPlan :: BuildPlan -> IndexBuildPlan
expectIndexPlan (BuildIndexPlan plan) = plan
expectIndexPlan _ = error "expected BuildIndexPlan"
