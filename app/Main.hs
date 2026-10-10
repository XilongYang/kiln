module Main (main) where

import Kiln.Init
import Kiln.Clean
import Kiln.Build

import Data.Version (showVersion)
import Paths_kiln (version)
import System.Environment (getArgs)
import System.Exit (die)

usage :: String
usage = "Usage: kiln (init [default|flow]|build|clean|help|version)"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]       -> Kiln.Init.kilnInit "default"
    ["init", name] -> Kiln.Init.kilnInit name
    ["clean"]      -> Kiln.Clean.kilnClean
    ["build"]      -> Kiln.Build.kilnBuild
    ["help"]       -> putStrLn usage
    ["version"]    -> putStrLn (showVersion version)
    []             -> Kiln.Build.kilnBuild
    _              -> die usage
