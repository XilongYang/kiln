module Kiln.Init (kilnInit) where

import Control.Monad (unless)
import Data.List (intercalate)
import Kiln.FileTree (copyTree)
import Paths_kiln (getDataDir)
import System.Directory (listDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | Every template `kiln init` can scaffold -- the directory name under
-- `template/` (both here and in this package's own @data-files@)
-- doubles as its name.
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
