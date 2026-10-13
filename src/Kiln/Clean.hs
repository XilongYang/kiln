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
-- Only files kiln itself wrote are removed, never a configured directory
-- wholesale. The cache directory is the one exception, because kiln owns
-- every byte of it. Everywhere else the config names a location kiln
-- *writes into*, not one it owns: @target.post.output-dir@ may be the
-- site root beside @index.html@ (and @"."@ is a perfectly reasonable
-- value for it), and @target.fonts.subset-dir@ may be a directory of
-- other assets. Removing those recursively -- as this used to -- turned
-- a reasonable config into a command that deleted the sources, the
-- templates and the config itself. Deriving the file list from what kiln
-- recorded building makes that impossible to reach, with nothing to
-- validate up front.
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

  -- Directories kiln created to hold those files have no reason to
  -- outlive them, so any that this emptied goes too -- but only if it is
  -- empty, which is what keeps a shared directory intact.
  mapM_
    pruneIfEmpty
    ( map (takeDirectory . pageOutput) (targetPages target)
        ++ maybe [] ((: []) . postPageOutputDir) (targetPost target)
        ++ maybe [] ((: []) . fontsSubsetDir) (targetFonts target)
    )

-- | Remove the page kiln rendered for each of `slugs`, and nothing else
-- in the directory.
removePostPages :: [String] -> PostPageConfig -> IO ()
removePostPages slugs ppc =
  mapM_ (\slug -> removeFileIfExists (postPageOutputDir ppc </> slug ++ ".html")) slugs

-- | Remove the @.woff2@ each configured font source subsets to, and
-- nothing else in the directory.
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

-- | Remove `path` if it is a directory kiln could have created and is
-- now empty. The current directory is never removed, however the config
-- spelled it: a site whose outputs live at its root is asking for the
-- files to go, not the project.
pruneIfEmpty :: FilePath -> IO ()
pruneIfEmpty path = do
  isDir <- doesDirectoryExist path
  unless (normalise path `elem` [".", "./", "", "/"] || not isDir) $ do
    empty <- null <$> listDirectory path
    when empty (removeDirectory path)
