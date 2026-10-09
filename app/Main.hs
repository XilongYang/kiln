module Main (main) where

import Kiln.Init
import Kiln.Clean
import Kiln.Build

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]       -> Kiln.Init.kilnInit "default"
    ["init", name] -> Kiln.Init.kilnInit name
    ["clean"]      -> Kiln.Clean.kilnClean
    ["build"]      -> Kiln.Build.kilnBuild
    []             -> Kiln.Build.kilnBuild
    _              -> die "Usage: kiln (init [default|flow]|clean|build)"
