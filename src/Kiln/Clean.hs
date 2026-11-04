module Kiln.Clean (kilnClean) where

import Control.Monad (unless, when)
import Kiln.Build.Post (postSlugs)
import Kiln.Config
  ( FontsConfig (..)
  , InputConfig (..)
  , KilnConfig (..)
  , PageConfig (..)
  , PostPageConfig (..)
  , TargetConfig (..)
  , readConfig
  )
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath (normalise, takeBaseName, takeDirectory, (</>))

-- | Remove everything @kiln build@ generated, the cache included, so the
-- next build is a full one.
--
-- The file list comes from what kiln recorded building, never from
-- listing a configured directory: the config names places kiln writes
-- into, not ones it owns -- @target.post.output-dir@ may be the site
-- root, @target.fonts.subset-dir@ a directory of other assets -- so a
-- recursive remove there would take the sources and the config with it.
-- The cache directory is the one exception, kiln owning every byte.
kilnClean :: IO ()
kilnClean = do
  config <- readConfig
  let target = configTarget config
      cacheDir = targetCacheDir target
  slugs <- postSlugs (inputSrcDir (configInput config)) cacheDir

  mapM_ removeFileIfExists (maybe [] (: []) (targetSearchDb target))
  mapM_ (removeFileIfExists . pageOutput) (targetPages target)
  mapM_ (removePostPages slugs) (targetPost target)
  mapM_ removeFontSubsets (targetFonts target)
  removeDirectoryIfExists cacheDir

  -- Directories kiln created for those files go too, but only if this
  -- emptied them, which is what keeps a shared directory intact.
  mapM_
    pruneIfEmpty
    ( map (takeDirectory . pageOutput) (targetPages target)
        ++ maybe [] ((: []) . postPageOutputDir) (targetPost target)
        ++ maybe [] ((: []) . fontsSubsetDir) (targetFonts target)
    )

-- | Remove only the pages kiln rendered for `slugs`, never the
-- directory.
removePostPages :: [String] -> PostPageConfig -> IO ()
removePostPages slugs ppc =
  mapM_ (\slug -> removeFileIfExists (postPageOutputDir ppc </> slug ++ ".html")) slugs

-- | Remove only the @.woff2@ each font source subsets to, never the
-- directory.
removeFontSubsets :: FontsConfig -> IO ()
removeFontSubsets fonts =
  mapM_
    (\src -> removeFileIfExists (fontsSubsetDir fonts </> takeBaseName src ++ ".woff2"))
    (fontsSources fonts)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  isFile <- doesFileExist path
  when isFile (removeFile path)

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists path = do
  isDir <- doesDirectoryExist path
  when isDir (removeDirectoryRecursive path)

-- | Remove `path` if it is an empty directory -- never the current one,
-- however the config spelled it: a site whose outputs live at its root
-- is asking for the files to go, not the project.
pruneIfEmpty :: FilePath -> IO ()
pruneIfEmpty path = do
  isDir <- doesDirectoryExist path
  unless (normalise path `elem` [".", "./", "", "/"] || not isDir) $ do
    empty <- null <$> listDirectory path
    when empty (removeDirectory path)
