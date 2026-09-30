module Main (main) where

import qualified Kiln.Version as Version

main :: IO ()
main = putStrLn ("kiln " ++ Version.version)
