module Modules.Utils.OrphanCheck (checkOrphans) where

import Control.Monad (unless)
import Data.List ((\\))
import Modules.Config

import System.Directory
  ( doesDirectoryExist
  , listDirectory
  )
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Detects generated post pages that no longer have matching source files.
--
-- This module provides a lightweight build-time hygiene check:
-- - scans generated post outputs under 'postPath'
-- - scans source markdown files under 'srcPath'
-- - reports HTML outputs whose basename is missing in sources
--
-- The check is warning-only and never fails the build. If the output
-- directory does not exist yet, it treats that as "nothing to check".

-- ---[ Public API ]------------------------------------------------------------

-- Checks the runtime filesystem and prints warning lines for orphaned output
-- pages. Missing output directory is treated as "nothing to check".
checkOrphans :: IO()
checkOrphans = do
  postPathExists <- doesDirectoryExist postPath
  if not postPathExists then return () else do
    postList <- listDirectory postPath
    srcList <- listDirectory srcPath

    let orphans = findOrphanPosts postPath postList srcList
    unless (null orphans) $ do
      putStrLn "[WARNING] Source file missing:"
      mapM_ print orphans

-- ---[ Implementation Details ]-----------------------------------------------

-- Returns generated post pages that do not have matching markdown sources.
--
-- Matching rule:
-- - generated posts are `*.html` under `postDir`
-- - source posts are `*.md` under `src`
-- - files are matched by basename only
findOrphanPosts :: FilePath -> [FilePath] -> [FilePath] -> [FilePath]
findOrphanPosts postDir postFiles srcFiles =
  let postNameList = map takeBaseName $ filter (\f -> takeExtension f == ".html") postFiles
      srcNameList = map takeBaseName $ filter (\f -> takeExtension f == ".md") srcFiles
      orphanNames = postNameList \\ srcNameList
  in map (\name -> postDir </> name <.> "html") orphanNames
