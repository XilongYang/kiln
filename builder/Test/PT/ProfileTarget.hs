module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (mapM_)
import Modules.BuildPlan (mkBuildIndexPlan, mkBuildPostPlan)
import Modules.Builder (executeBuildPlan)
import Modules.Config
import Modules.FontSubset (genFontSubset)
import Modules.Template (expandTemplate)
import Modules.Utils.Files (hashUpdate)
import Modules.Utils.OrphanCheck (checkOrphans)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, listDirectory, removePathForcibly)
import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(AppendMode), hPutStr, withFile)
import System.Process (callProcess)
import Test.Framework.Paths (withPrependedPath, withWorkDir)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [caseKey] -> runCase caseKey
    _ -> error "usage: profile-target <500-normal|500-one-changed|500-one-changed-warmup|5-huge>"

runCase :: String -> IO ()
runCase caseKey = do
  repoRoot <- getCurrentDirectory
  case caseKey of
    "500-normal" -> do
      let workRoot = workspacePath repoRoot "profile-500-normal"
      prepareWorkspace repoRoot "profile-500-normal" datasetNormalRelPath "hardlink"
      withPerfEnv workRoot runFullBuild
    "500-one-changed" -> do
      let workRoot = workspacePath repoRoot "profile-500-one-changed"
      -- The warmup full build is done outside this profiled run. This branch only
      -- measures the incremental rebuild after one tiny source change.
      withPerfEnv workRoot $ do
        appendTinyChange (srcPath </> "post-00001.md")
        resetTempDir
        threadDelay 1200000
        runFullBuild
    "500-one-changed-warmup" -> do
      let workRoot = workspacePath repoRoot "profile-500-one-changed"
      prepareWorkspace repoRoot "profile-500-one-changed" datasetNormalRelPath "copy"
      withPerfEnv workRoot runFullBuild
    "5-huge" -> do
      let workRoot = workspacePath repoRoot "profile-5-huge"
      prepareWorkspace repoRoot "profile-5-huge" datasetHugeRelPath "hardlink"
      withPerfEnv workRoot runFullBuild
    _ -> error "unknown case: expected one of 500-normal, 500-one-changed, 5-huge"

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

appendSearchItem :: FilePath -> IO ()
appendSearchItem path = do
  item <- readFile path
  appendFile searchDBPath item

listMarkdownSources :: IO [FilePath]
listMarkdownSources = do
  names <- listDirectory srcPath
  pure [srcPath </> name | name <- names, takeExtension name == ".md"]

datasetNormalRelPath :: FilePath
datasetNormalRelPath = ".cache" </> "PT" </> "500-normal-10k-v1" </> "src"

datasetHugeRelPath :: FilePath
datasetHugeRelPath = ".cache" </> "PT" </> "5-huge-5m-v1" </> "src"

workspacePath :: FilePath -> String -> FilePath
workspacePath repoRoot caseName = repoRoot </> ".cache" </> "PT" </> "workspaces" </> caseName

prepareWorkspace :: FilePath -> String -> FilePath -> String -> IO ()
prepareWorkspace repoRoot caseName datasetRelPath mode =
  callProcess
    "sh"
    [ repoRoot </> "builder" </> "Test" </> "PT" </> "scripts" </> "prepare-perf-workspace.sh"
    , repoRoot
    , caseName
    , datasetRelPath
    , mode
    ]

withPerfEnv :: FilePath -> IO a -> IO a
withPerfEnv workRoot action =
  withWorkDir workRoot $
    withPrependedPath (workRoot </> "bin") action

appendTinyChange :: FilePath -> IO ()
appendTinyChange path = withFile path AppendMode (\h -> hPutStr h " ")

resetTempDir :: IO ()
resetTempDir = do
  removePathForcibly tempPath
  createDirectoryIfMissing True tempPath
