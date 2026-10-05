module Main (main) where

import Kiln.Init
import Kiln.Clean

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]  -> Kiln.Init.kilnInit
    ["clean"] -> Kiln.Clean.kilnClean
    ["build"] -> die "build"
    []        -> die "build"
    _         -> die "Usage: kiln (init|clean|build)"

