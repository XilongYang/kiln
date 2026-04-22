module Test.PT.Modules.Performance (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Control.Monad (mapM_)
import Modules.BuildPlan (mkBuildIndexPlan, mkBuildPostPlan)
import Modules.Builder (executeBuildPlan)
import Modules.Config
import Modules.FontSubset (genFontSubset)
import Modules.Template (expandTemplate)
import Modules.Utils.Files (hashUpdate)
import Modules.Utils.OrphanCheck (checkOrphans)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getModificationTime
  , listDirectory
  , removePathForcibly
  )
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(AppendMode), hPutStr, withFile)
import System.Process (callProcess)
import Test.Framework.Asserts
import Test.Framework.Paths (withPrependedPath, withWorkDir)
import Test.Framework.Performance
import Test.Framework.TestSuite

suiteName :: String
suiteName = "Performance"

testCases :: [TestCase]
testCases =
  [ testBuild500NormalPosts
  , testBuild500PostsOneChanged
  , testBuild5HugePosts
  ]

testBuild500NormalPosts :: TestCase
testBuild500NormalPosts =
  mkTestCase "complete build: 500 normal posts (10 KiB each)" $ do
    repoRoot <- getCurrentDirectory
    let workRoot = workspacePath repoRoot "500-normal"
    prepareWorkspace repoRoot "500-normal" datasetNormalRelPath HardlinkMode
    withPerfEnv workRoot $ do
      (_, metrics) <- measurePerformance workRoot runFullBuild
      printPerformanceReport "500 normal posts (10 KiB each)" metrics
      firstPostExists <- doesFileExist (postPath </> "post-00001.html")
      lastPostExists <- doesFileExist (postPath </> "post-00500.html")
      assertTrue "build should generate first post html in performance case #1" firstPostExists
      assertTrue "build should generate last post html in performance case #1" lastPostExists

testBuild5HugePosts :: TestCase
testBuild5HugePosts =
  mkTestCase "complete build: 5 huge posts (5 MiB each)" $ do
    repoRoot <- getCurrentDirectory
    let workRoot = workspacePath repoRoot "10-huge"
    prepareWorkspace repoRoot "10-huge" datasetHugeRelPath HardlinkMode
    withPerfEnv workRoot $ do
      (_, metrics) <- measurePerformance workRoot runFullBuild
      printPerformanceReport "5 huge posts (5 MiB each)" metrics
      firstPostExists <- doesFileExist (postPath </> "post-00001.html")
      lastPostExists <- doesFileExist (postPath </> "post-00005.html")
      assertTrue "build should generate first huge post html in performance case #3" firstPostExists
      assertTrue "build should generate last huge post html in performance case #3" lastPostExists

testBuild500PostsOneChanged :: TestCase
testBuild500PostsOneChanged =
  mkTestCase "complete build with one changed source: 500 normal posts (10 KiB each)" $ do
    repoRoot <- getCurrentDirectory
    let workRoot = workspacePath repoRoot "500-one-changed"
    prepareWorkspace repoRoot "500-one-changed" datasetNormalRelPath DeepCopyMode
    withPerfEnv workRoot $ do
      runFullBuild
      let changedSrc = srcPath </> "post-00001.md"
      let changedHtml = postPath </> "post-00001.html"
      let stableHtml = postPath </> "post-00500.html"

      beforeChanged <- getModificationTime changedHtml
      beforeStable <- getModificationTime stableHtml
      threadDelay 1200000
      appendTinyChange changedSrc
      resetTempDir

      (_, metrics) <- measurePerformance workRoot runFullBuild
      printPerformanceReport "500 normal posts, one tiny source changed" metrics

      afterChanged <- getModificationTime changedHtml
      afterStable <- getModificationTime stableHtml
      assertTrue "changed post html should be rebuilt in performance case #2" (afterChanged > beforeChanged)
      assertEq "unchanged post html should not be rebuilt in performance case #2" beforeStable afterStable

runFullBuild :: IO ()
runFullBuild = do
  createDirectoryIfMissing True builderPath
  createDirectoryIfMissing True tempPath
  checkOrphans
  templatePost <- expandTemplate templatePostPath templateComponentPath
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- expandTemplate templateIndexPath templateComponentPath
  writeFile renderedTemplateIndexPath templateIndex
  createDirectoryIfMissing True postPath
  postPaths <- listMarkdownSources
  let postBuildPlans = map mkBuildPostPlan postPaths
  mapM_ executeBuildPlan postBuildPlans
  executeBuildPlan mkBuildIndexPlan
  writeFile searchDBPath ""
  searchItemNames <- listDirectory searchItemArtifactsPath
  let searchItemPaths = map (\f -> searchItemArtifactsPath </> f) $ filter (\f -> takeExtension f == ".klb") searchItemNames
  mapM_ appendSearchItem searchItemPaths
  genFontSubset
  hashUpdate builderPath builderStatePath

listMarkdownSources :: IO [FilePath]
listMarkdownSources = do
  names <- listDirectory srcPath
  pure [srcPath </> name | name <- names, takeExtension name == ".md"]

data CopyMode
  = HardlinkMode
  | DeepCopyMode

datasetNormalRelPath :: FilePath
datasetNormalRelPath = ".cache" </> "PT" </> "500-normal-10k-v1" </> "src"

datasetHugeRelPath :: FilePath
datasetHugeRelPath = ".cache" </> "PT" </> "5-huge-5m-v1" </> "src"

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

resetTempDir :: IO ()
resetTempDir = do
  removePathForcibly tempPath
  createDirectoryIfMissing True tempPath

withPerfEnv :: FilePath -> IO a -> IO a
withPerfEnv workRoot action =
  withWorkDir workRoot $
    withPrependedPath (workRoot </> "bin") action

appendSearchItem :: FilePath -> IO ()
appendSearchItem path = do
  item <- readFile path
  appendFile searchDBPath item
