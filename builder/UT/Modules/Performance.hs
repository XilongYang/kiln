module UT.Modules.Performance (suiteName, testCases) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (forM, forM_)
import Modules.BuildPlan (mkBuildIndexPlan, mkBuildPostPlan)
import Modules.Builder (executeBuildPlan)
import Modules.Config
import Modules.FontSubset (genFontSubset)
import Modules.Post (Post(..), PostMeta(..))
import Modules.Post.Parse (parsePost)
import Modules.SearchDB (genSearchDB)
import Modules.Template (expandTemplate)
import Modules.Utils.OrphanCheck (checkOrphans)
import Modules.Utils.TempDir (withTempDir)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getModificationTime
  , getTemporaryDirectory
  , listDirectory
  , setCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(AppendMode, WriteMode), Handle, hPutStr, withFile)
import System.Process (callProcess)
import UT.TestUtils.Asserts
import UT.TestUtils.Performance
import UT.TestUtils.TestSuite

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
  mkTestCase "complete build: 10000 normal posts (10 KiB each)" $
    withPerfCase
      "10000-normal"
      "complete build: 10000 normal posts (10 KiB each)"
      "10000 normal posts (10 KiB each)"
      (generatePosts 10000 (10 * 1024) writeNormalPost)
      (\_ -> do
          indexExists <- doesFileExist indexPath
          assertTrue "build should generate index.html in performance case #1" indexExists
      )

testBuild10HugePosts :: TestCase
testBuild10HugePosts =
  mkTestCase "complete build: 10 huge posts (10 MiB each)" $
    withPerfCase
      "10-huge"
      "complete build: 10 huge posts (10 MiB each)"
      "10 huge posts (100 MiB each)"
      (generatePosts 10 (10 * 1024 * 1024) writeHugePost)
      (\_ -> do
          searchExists <- doesFileExist searchDBPath
          assertTrue "build should generate searchdb.json in performance case #2" searchExists
      )

testBuild10000PostsOneChanged :: TestCase
testBuild10000PostsOneChanged =
  mkTestCase "complete build with one changed source: 10000 normal posts (10 KiB each)" $
    withPerfWorkspace "10000-one-changed" $ \workRoot -> do
      enabled <- ensurePerfEnabled "complete build with one changed source: 10000 normal posts (10 KiB each)"
      if not enabled
        then pure ()
        else do
          setupBuildWorkspace
          generatePosts 10000 (10 * 1024) writeNormalPost

          runFullBuild
          let changedSrc = srcPath </> "post-00001.md"
          let changedHtml = postPath </> "post-00001.html"
          let stableHtml = postPath </> "post-10000.html"

          beforeChanged <- getModificationTime changedHtml
          beforeStable <- getModificationTime stableHtml

          -- Ensure file-system timestamp granularity does not swallow this tiny edit.
          threadDelay 1200000
          appendTinyChange changedSrc

          (_, metrics) <- measurePerformance workRoot runFullBuild
          printPerformanceReport "10000 normal posts, one tiny source changed" metrics

          afterChanged <- getModificationTime changedHtml
          afterStable <- getModificationTime stableHtml
          assertTrue "changed post html should be rebuilt in performance case #3" (afterChanged > beforeChanged)
          assertEq "unchanged post html should not be rebuilt in performance case #3" beforeStable afterStable

withPerfWorkspace :: String -> (FilePath -> IO a) -> IO a
withPerfWorkspace caseName action = do
  tempRoot <- getTemporaryDirectory
  let workRoot = tempRoot </> ("xilong-site-ut-perf-" ++ caseName)
  let binDir = workRoot </> "bin"
  withTempDir workRoot $
    withWorkDir workRoot $ do
      createDirectoryIfMissing True binDir
      writeFakePandoc (binDir </> "pandoc")
      writeFakePyftsubset (binDir </> "pyftsubset")
      withPrependedPath binDir (action workRoot)

withPerfCase :: String -> String -> String -> IO () -> (PerfMetrics -> IO ()) -> IO ()
withPerfCase caseName title reportLabel setup verify =
  withPerfWorkspace caseName $ \workRoot -> do
    enabled <- ensurePerfEnabled title
    if not enabled
      then pure ()
      else do
        setupBuildWorkspace
        setup
        (_, metrics) <- measurePerformance workRoot runFullBuild
        printPerformanceReport reportLabel metrics
        verify metrics

setupBuildWorkspace :: IO ()
setupBuildWorkspace = do
  createDirectoryIfMissing True srcPath
  createDirectoryIfMissing True templateComponentPath
  createDirectoryIfMissing True fontPath
  writeFile templatePostPath fixturePostTemplate
  writeFile templateIndexPath fixtureIndexTemplate
  writeFile originFontFilePath "fake-otf"

runFullBuild :: IO ()
runFullBuild = withTempDir tempPath $ do
  checkOrphans
  templatePost <- expandTemplate templatePostPath templateComponentPath
  writeFile renderedTemplatePostPath templatePost
  templateIndex <- expandTemplate templateIndexPath templateComponentPath
  writeFile renderedTemplateIndexPath templateIndex
  createDirectoryIfMissing True postPath

  postPaths <- listMarkdownSources
  slimPosts <- forM postPaths $ \path -> do
    post <- parsePost path
    executeBuildPlan (mkBuildPostPlan post)
    pure (slimPost post)

  executeBuildPlan (mkBuildIndexPlan slimPosts)
  genSearchDB searchDBPath slimPosts
  genFontSubset

listMarkdownSources :: IO [FilePath]
listMarkdownSources = do
  names <- listDirectory srcPath
  pure [srcPath </> name | name <- names, takeExtension name == ".md"]

slimPost :: Post -> Post
slimPost post =
  post
    { postBody = ""
    , postAbstract = ""
    }

generatePosts :: Int -> Int -> (FilePath -> Int -> Int -> IO ()) -> IO ()
generatePosts count targetBytes writer =
  forM_ [1 .. count] $ \idx -> do
    let path = srcPath </> postNameFromIndex idx
    writer path idx targetBytes

postNameFromIndex :: Int -> FilePath
postNameFromIndex idx = "post-" ++ leftPad5 idx ++ ".md"

leftPad5 :: Int -> String
leftPad5 idx =
  let s = show idx
   in replicate (max 0 (5 - length s)) '0' ++ s

writeNormalPost :: FilePath -> Int -> Int -> IO ()
writeNormalPost path idx targetBytes = do
  let header = mkPostHeader idx
  let required = max 0 (targetBytes - length header)
  let body = take required (cycle normalBodyChunk)
  writeFile path (header ++ body)

writeHugePost :: FilePath -> Int -> Int -> IO ()
writeHugePost path idx targetBytes = do
  let header = mkPostHeader idx
  withFile path WriteMode $ \h -> do
    hPutStr h header
    let remaining = max 0 (targetBytes - length header)
    writeRepeated h hugeBodyChunk remaining

writeRepeated :: Handle -> String -> Int -> IO ()
writeRepeated _ _ n | n <= 0 = pure ()
writeRepeated h chunk n = do
  let chunkLen = length chunk
  if n >= chunkLen
    then do
      hPutStr h chunk
      writeRepeated h chunk (n - chunkLen)
    else hPutStr h (take n chunk)

appendTinyChange :: FilePath -> IO ()
appendTinyChange path = withFile path AppendMode (\h -> hPutStr h " ")

mkPostHeader :: Int -> String
mkPostHeader idx =
  unlines
    [ "---"
    , "title: Perf Post " ++ show idx
    , "author: Perf UT"
    , "date: 2026-03-28"
    , "---"
    , ""
    , "This is abstract content."
    , ""
    , "## Section A"
    , ""
    ]

normalBodyChunk :: String
normalBodyChunk = "normal-body-line-abcdefghijklmnopqrstuvwxyz0123456789\n"

hugeBodyChunk :: String
hugeBodyChunk = "huge-body-line-abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"

fixturePostTemplate :: String
fixturePostTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body><main>$body$</main></body></html>"
    ]

fixtureIndexTemplate :: String
fixtureIndexTemplate =
  unlines
    [ "<!doctype html>"
    , "<html><body>$posts$</body></html>"
    ]

writeFakePandoc :: FilePath -> IO ()
writeFakePandoc scriptPath =
  writeExecutableScript scriptPath $
    unlines
      [ "#!/bin/sh"
      , "in=\"$1\""
      , "out=\"\""
      , "plain=0"
      , "while [ \"$#\" -gt 0 ]; do"
      , "  case \"$1\" in"
      , "    -o) shift; out=\"$1\" ;;"
      , "    -t) shift; [ \"$1\" = \"plain\" ] && plain=1 ;;"
      , "  esac"
      , "  shift"
      , "done"
      , "if [ \"$plain\" -eq 1 ]; then"
      , "  cat \"$in\""
      , "  exit 0"
      , "fi"
      , "if [ -n \"$out\" ]; then"
      , "  {"
      , "    echo \"<main>\""
      , "    cat \"$in\""
      , "    echo \"</main>\""
      , "  } > \"$out\""
      , "fi"
      ]

writeFakePyftsubset :: FilePath -> IO ()
writeFakePyftsubset scriptPath =
  writeExecutableScript scriptPath $
    unlines
      [ "#!/bin/sh"
      , "for arg in \"$@\"; do"
      , "  case \"$arg\" in"
      , "    --output-file=*) out=\"${arg#--output-file=}\" ;;"
      , "  esac"
      , "done"
      , "[ -n \"$out\" ] && : > \"$out\""
      ]

writeExecutableScript :: FilePath -> String -> IO ()
writeExecutableScript scriptPath content = do
  writeFile scriptPath content
  callProcess "chmod" ["+x", scriptPath]

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
