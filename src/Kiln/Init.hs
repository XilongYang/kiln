module Kiln.Init (kilnInit) where

import Control.Monad (unless)
import Kiln.FileTree (copyTree)
import Paths_kiln (getDataDir)
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath ((</>))

kilnInit :: IO ()
kilnInit = do
  empty <- isEmptyDir "."
  unless empty $ die "Directory is not empty"
  dataDir <- getDataDir
  copyTree (dataDir </> "template" </> "hello-kiln") "."

isEmptyDir :: FilePath -> IO Bool
isEmptyDir p = null <$> listDirectory p
