module Test.Framework.Performance
  ( PerfMetrics(..)
  , measurePerformance
  , printPerformanceReport
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Exception (Exception, finally, throwIO, throwTo)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
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
import System.IO (hPutStrLn, stderr)

data PerfMemoryLimitExceeded = PerfMemoryLimitExceeded
  { memLimitExceededActualKb :: Integer
  , memLimitExceededLimitKb :: Integer
  }
  deriving (Show, Typeable)

instance Exception PerfMemoryLimitExceeded

perfMemoryLimitKb :: Integer
perfMemoryLimitKb = 5 * 1024 * 1024

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

  if rssBefore > perfMemoryLimitKb
    then throwIO (PerfMemoryLimitExceeded rssBefore perfMemoryLimitKb)
    else pure ()

  peakRef <- newIORef rssBefore
  mainTid <- myThreadId
  samplerTid <- forkIO (samplePeakRssWithLimit mainTid peakRef perfMemoryLimitKb)

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
  hPutStrLn stderr ("[PERF] " ++ label)
  hPutStrLn stderr
    ("  Time : wall=" ++ showMsWithHuman (perfWallMs metrics) ++ ", cpu=" ++ showMsWithHuman (perfCpuMs metrics))
  hPutStrLn stderr
    ("  IO   : read-char=" ++ showBytesWithHuman (perfIoReadChars metrics) ++ ", write-char=" ++ showBytesWithHuman (perfIoWriteChars metrics))
  hPutStrLn stderr ("  CPU  : process-cpu=" ++ showMsWithHuman (perfCpuMs metrics))
  hPutStrLn stderr
    ("  Mem  : peak-rss=" ++ showKiBWithHuman (perfMemRssPeakKb metrics) ++ ", delta-rss=" ++ showKiBWithHuman (perfMemRssDeltaKb metrics))
  hPutStrLn stderr
    ("  Disk : read=" ++ showBytesWithHuman (perfDiskReadBytes metrics) ++ ", write=" ++ showBytesWithHuman (perfDiskWriteBytes metrics) ++ ", workspace-delta=" ++ showBytesWithHuman (perfWorkspaceDeltaBytes metrics))

showFixed1 :: Double -> String
showFixed1 value =
  let scaled :: Integer
      scaled = round (value * 10)
      integerPart = scaled `div` 10
      decimalPart = abs (scaled `mod` 10)
   in show integerPart ++ "." ++ show decimalPart

showMsWithHuman :: Double -> String
showMsWithHuman ms =
  showFixed1 ms ++ " ms (" ++ showFixed1 (ms / 1000.0) ++ " s)"

showBytesWithHuman :: Integer -> String
showBytesWithHuman bytes =
  show bytes ++ " B (" ++ showIecBytes bytes ++ ")"

showKiBWithHuman :: Integer -> String
showKiBWithHuman kib =
  show kib ++ " KiB (" ++ showIecFromKiB kib ++ ")"

showIecBytes :: Integer -> String
showIecBytes bytes =
  let sign = if bytes < 0 then "-" else ""
      absBytes = abs bytes
      kib = fromIntegral absBytes / 1024.0 :: Double
      mib = kib / 1024.0
      gib = mib / 1024.0
   in if absBytes < 1024
        then sign ++ show absBytes ++ " B"
        else
          if absBytes < 1024 * 1024
            then sign ++ showFixed1 kib ++ " KiB"
            else
              if absBytes < 1024 * 1024 * 1024
                then sign ++ showFixed1 mib ++ " MiB"
                else sign ++ showFixed1 gib ++ " GiB"

showIecFromKiB :: Integer -> String
showIecFromKiB kib =
  let sign = if kib < 0 then "-" else ""
      absKiB = abs kib
      mib = fromIntegral absKiB / 1024.0 :: Double
      gib = mib / 1024.0
   in if absKiB < 1024
        then sign ++ show absKiB ++ " KiB"
        else
          if absKiB < 1024 * 1024
            then sign ++ showFixed1 mib ++ " MiB"
            else sign ++ showFixed1 gib ++ " GiB"

samplePeakRssWithLimit :: ThreadId -> IORef Integer -> Integer -> IO ()
samplePeakRssWithLimit mainTid peakRef limitKb = do
  rss <- readVmRssKb
  atomicModifyIORef' peakRef (\old -> (max old rss, ()))
  if rss > limitKb
    then throwTo mainTid (PerfMemoryLimitExceeded rss limitKb)
    else pure ()
  threadDelay 50000
  samplePeakRssWithLimit mainTid peakRef limitKb

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
