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
genTemplate template = do
  templateHtml <- readFile template
  components <- loadComponents
  return (expandComponents templateHtml components)

loadComponents :: IO [(ComponentName, ComponentHtml)]
loadComponents = do
  componentFiles <- listDirectory templateComponentPath
  let htmlFiles = filter (\f -> takeExtension f == ".html") componentFiles
  mapM loadComponent(sort htmlFiles)

loadComponent :: FilePath -> IO (ComponentName, ComponentHtml)
loadComponent filename = do
  let fullPath = templateComponentPath </> filename
  html <- readFile fullPath 
  return (takeBaseName filename, html)

expandComponents :: TemplateHtml -> [(ComponentName, ComponentHtml)] -> TemplateHtml
expandComponents templateHtml componentList = foldl replaceComponents templateHtml componentList
  where 
    replaceComponents templateHtml' (componentName, componentHtml) = 
      replace (placeholder componentName) componentHtml templateHtml'
    placeholder componentName = replace componentPlaceholderToken componentName componentPlaceholderPattern

