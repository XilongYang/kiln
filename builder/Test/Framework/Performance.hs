module Test.Framework.Performance
  ( PerfMetrics(..)
  , measurePerformance
  , printPerformanceReport
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (performMajorGC)
import System.CPUTime (getCPUTime)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getFileSize
  , listDirectory
  )
import System.FilePath ((</>))

data PerfMetrics = PerfMetrics
  { perfWallMs :: Double
  , perfCpuMs :: Double
  , perfIoReadChars :: Integer
  , perfIoWriteChars :: Integer
  , perfDiskReadBytes :: Integer
  , perfDiskWriteBytes :: Integer
  , perfMemRssPeakKb :: Integer
  , perfMemRssDeltaKb :: Integer
  , perfWorkspaceDeltaBytes :: Integer
  }

data ProcIo = ProcIo
  { ioRchar :: Integer
  , ioWchar :: Integer
  , ioReadBytes :: Integer
  , ioWriteBytes :: Integer
  }

measurePerformance :: FilePath -> IO a -> IO (a, PerfMetrics)
measurePerformance workspace action = do
  -- Reduce memory carry-over from setup phase before baseline sampling.
  performMajorGC
  performMajorGC
  threadDelay 50000

  workspaceBefore <- directorySize workspace
  ioBefore <- readProcIo
  rssBefore <- readVmRssKb

  peakRef <- newIORef rssBefore
  samplerTid <- forkIO (samplePeakRss peakRef)

  wallStartNs <- getMonotonicTimeNSec
  cpuStartPs <- getCPUTime
  result <- action `finally` killThread samplerTid
  cpuEndPs <- getCPUTime
  wallEndNs <- getMonotonicTimeNSec

  workspaceAfter <- directorySize workspace
  ioAfter <- readProcIo
  rssAfter <- readVmRssKb
  rssPeak <- readIORef peakRef

  let metrics =
        PerfMetrics
          { perfWallMs = fromIntegral (wallEndNs - wallStartNs) / 1000000.0
          , perfCpuMs = fromIntegral (cpuEndPs - cpuStartPs) / 1000000000.0
          , perfIoReadChars = ioRchar ioAfter - ioRchar ioBefore
          , perfIoWriteChars = ioWchar ioAfter - ioWchar ioBefore
          , perfDiskReadBytes = ioReadBytes ioAfter - ioReadBytes ioBefore
          , perfDiskWriteBytes = ioWriteBytes ioAfter - ioWriteBytes ioBefore
          , perfMemRssPeakKb = max rssPeak rssAfter
          , perfMemRssDeltaKb = rssAfter - rssBefore
          , perfWorkspaceDeltaBytes = workspaceAfter - workspaceBefore
          }
  pure (result, metrics)

printPerformanceReport :: String -> PerfMetrics -> IO ()
printPerformanceReport label metrics = do
  putStrLn ("[PERF] " ++ label)
  putStrLn ("  Time : wall=" ++ showFixed1 (perfWallMs metrics) ++ " ms, cpu=" ++ showFixed1 (perfCpuMs metrics) ++ " ms")
  putStrLn ("  IO   : read-char=" ++ show (perfIoReadChars metrics) ++ " B, write-char=" ++ show (perfIoWriteChars metrics) ++ " B")
  putStrLn ("  CPU  : process-cpu=" ++ showFixed1 (perfCpuMs metrics) ++ " ms")
  putStrLn ("  Mem  : peak-rss=" ++ show (perfMemRssPeakKb metrics) ++ " KiB, delta-rss=" ++ show (perfMemRssDeltaKb metrics) ++ " KiB")
  putStrLn ("  Disk : read=" ++ show (perfDiskReadBytes metrics) ++ " B, write=" ++ show (perfDiskWriteBytes metrics) ++ " B, workspace-delta=" ++ show (perfWorkspaceDeltaBytes metrics) ++ " B")

showFixed1 :: Double -> String
showFixed1 value =
  let scaled :: Integer
      scaled = round (value * 10)
      integerPart = scaled `div` 10
      decimalPart = abs (scaled `mod` 10)
   in show integerPart ++ "." ++ show decimalPart

samplePeakRss :: IORef Integer -> IO ()
samplePeakRss peakRef = do
  rss <- readVmRssKb
  atomicModifyIORef' peakRef (\old -> (max old rss, ()))
  threadDelay 50000
  samplePeakRss peakRef

readProcIo :: IO ProcIo
readProcIo = do
  content <- readProcText "/proc/self/io"
  pure $
    ProcIo
      { ioRchar = parseProcValue "rchar:" content
      , ioWchar = parseProcValue "wchar:" content
      , ioReadBytes = parseProcValue "read_bytes:" content
      , ioWriteBytes = parseProcValue "write_bytes:" content
      }

readVmRssKb :: IO Integer
readVmRssKb = do
  content <- readProcText "/proc/self/status"
  pure (parseProcValue "VmRSS:" content)

readProcText :: FilePath -> IO String
readProcText path = BS.unpack <$> BS.readFile path

parseProcValue :: String -> String -> Integer
parseProcValue key content =
  case filter (isPrefixOf key) (lines content) of
    [] -> 0
    (line:_) ->
      case words line of
        (_:value:_) ->
          case reads value of
            [(n, "")] -> n
            _ -> 0
        _ -> 0

directorySize :: FilePath -> IO Integer
directorySize path = do
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      names <- listDirectory path
      childSizes <- mapM (directoryEntrySize . (path </>)) names
      pure (sum childSizes)
    else do
      fileExists <- doesFileExist path
      if fileExists
        then getFileSize path
        else pure 0

directoryEntrySize :: FilePath -> IO Integer
directoryEntrySize path = do
  isDir <- doesDirectoryExist path
  if isDir
    then directorySize path
    else do
      exists <- doesFileExist path
      if exists then getFileSize path else pure 0
