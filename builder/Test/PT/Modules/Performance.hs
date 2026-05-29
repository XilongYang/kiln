module Test.PT.Modules.Performance (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (mapM_)
import Modules.BuildPlan (mkBuildPostPlan)
import Modules.Builder (executeBuildPlan)
import Modules.Config
import Modules.Template (expandTemplate)
import Modules.Utils.OrphanCheck (checkOrphans)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getModificationTime
  , listDirectory
  , setCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(AppendMode), hPutStr, withFile)
import System.Process (callProcess)
import Test.Framework.Asserts
import Test.Framework.Performance
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Performance"

testCases :: [TestCase]
testCases =
  [ testBuild10000NormalPosts
  , testBuild10HugePosts
  , testBuild10000PostsOneChanged
  ]

testBuild10000NormalPosts :: TestCase
testBuild10000NormalPosts =
  mkTestCase "complete build: 10000 normal posts (10 KiB each)" $ do
    enabled <- ensurePerfEnabled "complete build: 10000 normal posts (10 KiB each)"
    if not enabled
      then pure ()
      else do
        repoRoot <- getCurrentDirectory
        let workRoot = workspacePath repoRoot "10000-normal"
        prepareWorkspace repoRoot "10000-normal" datasetNormalRelPath HardlinkMode
        withPerfEnv workRoot $ do
          (_, metrics) <- measurePerformance workRoot runFullBuild
          printPerformanceReport "10000 normal posts (10 KiB each)" metrics
          firstPostExists <- doesFileExist (postPath </> "post-00001.html")
          lastPostExists <- doesFileExist (postPath </> "post-10000.html")
          assertTrue "build should generate first post html in performance case #1" firstPostExists
          assertTrue "build should generate last post html in performance case #1" lastPostExists

testBuild10HugePosts :: TestCase
testBuild10HugePosts =
  mkTestCase "complete build: 5 huge posts (25 MiB each)" $ do
    enabled <- ensurePerfEnabled "complete build: 5 huge posts (25 MiB each)"
    if not enabled
      then pure ()
      else do
        repoRoot <- getCurrentDirectory
        let workRoot = workspacePath repoRoot "10-huge"
        prepareWorkspace repoRoot "10-huge" datasetHugeRelPath HardlinkMode
        withPerfEnv workRoot $ do
          (_, metrics) <- measurePerformance workRoot runFullBuild
          printPerformanceReport "5 huge posts (25 MiB each)" metrics
          firstPostExists <- doesFileExist (postPath </> "post-00001.html")
          lastPostExists <- doesFileExist (postPath </> "post-00005.html")
          assertTrue "build should generate first huge post html in performance case #2" firstPostExists
          assertTrue "build should generate last huge post html in performance case #2" lastPostExists

testBuild10000PostsOneChanged :: TestCase
testBuild10000PostsOneChanged =
  mkTestCase "complete build with one changed source: 10000 normal posts (10 KiB each)" $ do
    enabled <- ensurePerfEnabled "complete build with one changed source: 10000 normal posts (10 KiB each)"
    if not enabled
      then pure ()
      else do
        repoRoot <- getCurrentDirectory
        let workRoot = workspacePath repoRoot "10000-one-changed"
        prepareWorkspace repoRoot "10000-one-changed" datasetNormalRelPath DeepCopyMode
        withPerfEnv workRoot $ do
          runFullBuild
          let changedSrc = srcPath </> "post-00001.md"
          let changedHtml = postPath </> "post-00001.html"
          let stableHtml = postPath </> "post-10000.html"

          beforeChanged <- getModificationTime changedHtml
          beforeStable <- getModificationTime stableHtml
          threadDelay 1200000
          appendTinyChange changedSrc

          (_, metrics) <- measurePerformance workRoot runFullBuild
          printPerformanceReport "10000 normal posts, one tiny source changed" metrics

          afterChanged <- getModificationTime changedHtml
          afterStable <- getModificationTime stableHtml
          assertTrue "changed post html should be rebuilt in performance case #3" (afterChanged > beforeChanged)
          assertEq "unchanged post html should not be rebuilt in performance case #3" beforeStable afterStable

runFullBuild :: IO ()
runFullBuild = do
  createDirectoryIfMissing True tempPath
  checkOrphans
  templatePost <- expandTemplate templatePostPath templateComponentPath
  writeFile renderedTemplatePostPath templatePost
  createDirectoryIfMissing True postPath
  postPaths <- listMarkdownSources
  let postBuildPlans = map mkBuildPostPlan postPaths
  mapM_ executeBuildPlan postBuildPlans

listMarkdownSources :: IO [FilePath]
listMarkdownSources = do
  names <- listDirectory srcPath
  pure [srcPath </> name | name <- names, takeExtension name == ".md"]

data CopyMode
  = HardlinkMode
  | DeepCopyMode

datasetNormalRelPath :: FilePath
datasetNormalRelPath = ".cache" </> "PT" </> "10000-normal-10k-v1" </> "src"

datasetHugeRelPath :: FilePath
datasetHugeRelPath = ".cache" </> "PT" </> "5-huge-25m-v1" </> "src"

workspacePath :: FilePath -> String -> FilePath
workspacePath repoRoot caseName = repoRoot </> ".cache" </> "PT" </> "workspaces" </> caseName

prepareWorkspace :: FilePath -> String -> FilePath -> CopyMode -> IO ()
prepareWorkspace repoRoot caseName datasetRelPath mode =
  callProcess
    "sh"
    [ repoRoot </> "builder" </> "Test" </> "PT" </> "scripts" </> "prepare-perf-workspace.sh"
    , repoRoot
    , caseName
    , datasetRelPath
    , modeArg mode
    ]
  where
    modeArg HardlinkMode = "hardlink"
    modeArg DeepCopyMode = "copy"

appendTinyChange :: FilePath -> IO ()
appendTinyChange path = withFile path AppendMode (\h -> hPutStr h " ")

withPerfEnv :: FilePath -> IO a -> IO a
withPerfEnv workRoot action =
  withWorkDir workRoot $
    withPrependedPath (workRoot </> "bin") action

withWorkDir :: FilePath -> IO a -> IO a
withWorkDir dir action = do
  old <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory old) action

withPrependedPath :: FilePath -> IO a -> IO a
withPrependedPath path action = do
  old <- lookupEnv "PATH"
  let newPath = case old of
        Just existing -> path ++ ":" ++ existing
        Nothing -> path
  bracket_ (setEnv "PATH" newPath) (restore old) action
  where
    restore (Just value) = setEnv "PATH" value
    restore Nothing = unsetEnv "PATH"

ensurePerfEnabled :: String -> IO Bool
ensurePerfEnabled title = do
  enabled <- lookupEnv "UT_ENABLE_PERF"
  case enabled of
    Just "1" -> pure True
    _ -> do
      putStrLn ("[PERF][SKIP] " ++ title ++ " (set UT_ENABLE_PERF=1 to run)")
      pure False
