module Modules.Template (expandTemplate) where

import Data.List (foldl', sort)
import Modules.Config
import Modules.Utils.String
import Modules.TypeAlias

import System.Directory (listDirectory)
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------

-- | Template renderer for component-based HTML pages.
--
-- A template can contain component placeholders (for example @<!--<navbar>-->@).
-- During rendering, each placeholder is replaced by the HTML loaded from the
-- corresponding component file (for example @navbar.html@).
--
-- Expansion behavior guarantees:
-- - only lowercase @.html@ component files are loaded
-- - component expansion order is deterministic (sorted by filename)
-- - unknown placeholders are left unchanged

-- ---[ Public API ]------------------------------------------------------------

-- | Render a template file by expanding placeholders with HTML components.
--
-- Parameters:
-- - template file path
-- - component directory path
--
-- Throws I/O exceptions when template or component files/directories are missing.
expandTemplate :: FilePath -> FilePath -> IO Html
expandTemplate template componentPath = do
  templateHtml <- readFile template
  components <- loadComponentsFrom componentPath
  return (expandComponents templateHtml components)

-- ---[ Implementation Details ]-----------------------------------------------

type ComponentName = String

-- | Load all eligible components from a directory.
--
-- The loader keeps only files with extension @.html@, sorts filenames, and
-- returns @(componentName, componentHtml)@ tuples.
loadComponentsFrom :: FilePath -> IO [(ComponentName, Html)]
loadComponentsFrom componentDir = do
  componentFiles <- listDirectory componentDir
  let htmlFiles = filter (\f -> takeExtension f == ".html") componentFiles
  mapM (loadComponent componentDir) (sort htmlFiles)
  where
    loadComponent :: FilePath -> FilePath -> IO (String, Html)
    loadComponent componentDir filename = do
      let fullPath = componentDir </> filename
      html <- readFile fullPath 
      return (takeBaseName filename, html)

-- | Expand known component placeholders in template HTML.
--
-- This function is pure and deterministic for a fixed input list.
expandComponents :: Html -> [(ComponentName, Html)] -> Html
expandComponents templateHtml componentList = foldl' replaceComponents templateHtml componentList
  where 
    replaceComponents :: Html -> (ComponentName, Html) -> Html
    replaceComponents templateHtml' (componentName, componentHtml) = 
      replace (mkPlaceholder componentName) componentHtml templateHtml'

    mkPlaceholder :: ComponentName -> String
    mkPlaceholder componentName = replace componentPlaceholderToken componentName componentPlaceholderPattern
