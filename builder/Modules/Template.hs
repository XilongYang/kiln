module Modules.Template where

import Modules.Config
import Modules.Utils.String

import System.Directory (listDirectory)
import System.FilePath
import System.IO (readFile)
import Data.List (sort)

-- HTML content after all component placeholders have been expanded.
type TemplateHtml = String

-- Logical component identifier derived from the source filename stem.
type ComponentName = String

-- Raw HTML fragment loaded from a component file.
type ComponentHtml = String

-- Production entry point for template generation.
--
-- This uses the repository's configured component directory and expands all
-- known placeholders in the target template file.
genTemplate :: FilePath -> IO TemplateHtml
genTemplate = genTemplateFrom templateComponentPath

-- Testable template generation entry point with injectable component directory.
--
-- Keeping the component path explicit allows UT to run against isolated mock
-- data without depending on the real template tree.
genTemplateFrom :: FilePath -> FilePath -> IO TemplateHtml
genTemplateFrom componentDir template = do
  templateHtml <- readFile template
  components <- loadComponentsFrom componentDir
  return (expandComponents templateHtml components)

-- Loads every HTML component from the configured production component
-- directory.
loadComponents :: IO [(ComponentName, ComponentHtml)]
loadComponents = loadComponentsFrom templateComponentPath

-- Loads all `.html` component files from the given directory.
--
-- The file list is sorted to keep expansion order deterministic across runs.
loadComponentsFrom :: FilePath -> IO [(ComponentName, ComponentHtml)]
loadComponentsFrom componentDir = do
  componentFiles <- listDirectory componentDir
  let htmlFiles = filter (\f -> takeExtension f == ".html") componentFiles
  mapM (loadComponentFrom componentDir) (sort htmlFiles)

-- Loads a single component from the configured production component directory.
loadComponent :: FilePath -> IO (ComponentName, ComponentHtml)
loadComponent = loadComponentFrom templateComponentPath

-- Loads one component file and derives its logical component name from the
-- filename stem, e.g. `navbar.html -> navbar`.
loadComponentFrom :: FilePath -> FilePath -> IO (ComponentName, ComponentHtml)
loadComponentFrom componentDir filename = do
  let fullPath = componentDir </> filename
  html <- readFile fullPath 
  return (takeBaseName filename, html)

-- Pure placeholder expansion used by both production code and UT.
--
-- Each component replaces placeholders of the form `<!--<name>-->` in the
-- template HTML.
expandComponents :: TemplateHtml -> [(ComponentName, ComponentHtml)] -> TemplateHtml
expandComponents templateHtml componentList = foldl replaceComponents templateHtml componentList
  where 
    replaceComponents templateHtml' (componentName, componentHtml) = 
      replace (placeholder componentName) componentHtml templateHtml'
    placeholder componentName = replace componentPlaceholderToken componentName componentPlaceholderPattern
