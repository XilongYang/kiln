module Kiln.Init (kilnInit) where

import Control.Monad (unless)
import Data.List (intercalate)
import Kiln.FileTree (copyTree)
import Paths_kiln (getDataDir)
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | Templates `kiln init` can scaffold; each name is its directory under
-- @template\/@, here and in this package's @data-files@.
templateNames :: [String]
templateNames = ["default", "flow"]

kilnInit :: String -> IO ()
kilnInit name = do
  unless (name `elem` templateNames) $
    die ("Unknown template: " ++ name ++ " (choices: " ++ intercalate ", " templateNames ++ ")")
  empty <- isEmptyDir "."
  unless empty $ die "Directory is not empty"
  dataDir <- getDataDir
  copyTree (dataDir </> "template" </> name) "."

isEmptyDir :: FilePath -> IO Bool
isEmptyDir p = null <$> listDirectory p
