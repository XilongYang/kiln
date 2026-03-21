module Modules.Template where

import Modules.Config
import Modules.Utils.String

import System.Directory (listDirectory)
import System.FilePath
import System.IO (readFile)
import Data.List (sort)

type TemplateHtml = String
type ComponentName = String
type ComponentHtml = String

genTemplate :: FilePath -> IO TemplateHtml
genTemplate = genTemplateFrom templateComponentPath

genTemplateFrom :: FilePath -> FilePath -> IO TemplateHtml
genTemplateFrom componentDir template = do
  templateHtml <- readFile template
  components <- loadComponentsFrom componentDir
  return (expandComponents templateHtml components)

loadComponents :: IO [(ComponentName, ComponentHtml)]
loadComponents = loadComponentsFrom templateComponentPath

loadComponentsFrom :: FilePath -> IO [(ComponentName, ComponentHtml)]
loadComponentsFrom componentDir = do
  componentFiles <- listDirectory componentDir
  let htmlFiles = filter (\f -> takeExtension f == ".html") componentFiles
  mapM (loadComponentFrom componentDir) (sort htmlFiles)

loadComponent :: FilePath -> IO (ComponentName, ComponentHtml)
loadComponent = loadComponentFrom templateComponentPath

loadComponentFrom :: FilePath -> FilePath -> IO (ComponentName, ComponentHtml)
loadComponentFrom componentDir filename = do
  let fullPath = componentDir </> filename
  html <- readFile fullPath 
  return (takeBaseName filename, html)

expandComponents :: TemplateHtml -> [(ComponentName, ComponentHtml)] -> TemplateHtml
expandComponents templateHtml componentList = foldl replaceComponents templateHtml componentList
  where 
    replaceComponents templateHtml' (componentName, componentHtml) = 
      replace (placeholder componentName) componentHtml templateHtml'
    placeholder componentName = replace componentPlaceholderToken componentName componentPlaceholderPattern
