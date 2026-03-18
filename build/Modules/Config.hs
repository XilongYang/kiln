module Modules.Config where

import System.FilePath
import Data.Array (Ix(index))

-- Root
rootPath :: FilePath
rootPath = "."

-- Source
srcPath :: FilePath
srcPath = rootPath </> "src"

-- Target
postPath :: FilePath
postPath = rootPath </> "posts"

indexPath :: FilePath
indexPath = rootPath </> "index.html"

-- Temp
tempPath :: FilePath
tempPath = rootPath </> "temp"

tempTemplatePostPath :: FilePath
tempTemplatePostPath = tempPath </> "post.html"

tempTemplateIndexPath :: FilePath
tempTemplateIndexPath = tempPath </> "index.html"

-- Template
templatePath :: FilePath
templatePath = rootPath </> "template"

templateIndexPath :: FilePath
templateIndexPath = templatePath </> "index.html"

templatePostPath :: FilePath
templatePostPath = templatePath </> "post.html"

templateComponentPath :: FilePath
templateComponentPath = templatePath </> "component"

componentPlaceholderPattern :: String
componentPlaceholderPattern = "<!--<" ++ componentPlaceholderToken ++ ">-->"

componentPlaceholderToken :: String
componentPlaceholderToken= "name"

-- Web
webRoot = "/"
webPost = webRoot ++ "posts"

