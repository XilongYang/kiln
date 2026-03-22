module Modules.Config where

import System.FilePath

-- Centralized project paths and placeholder conventions used by the build
-- pipeline.
--
-- Keeping these values in one module avoids scattering path construction logic
-- across generators, template loaders, and UT code.

-- Repository root for all relative build paths.
rootPath :: FilePath
rootPath = "."

-- Directory containing source markdown posts.
srcPath :: FilePath
srcPath = rootPath </> "src"

-- Output directory for generated post pages.
postPath :: FilePath
postPath = rootPath </> "posts"

-- Output path for the generated site index page.
indexPath :: FilePath
indexPath = rootPath </> "index.html"

-- Scratch directory used during generation for intermediate artifacts.
tempPath :: FilePath
tempPath = rootPath </> "temp"

-- Temporary rendered post template passed to downstream tools.
tempTemplatePostPath :: FilePath
tempTemplatePostPath = tempPath </> "post.html"

-- Temporary rendered index template.
tempTemplateIndexPath :: FilePath
tempTemplateIndexPath = tempPath </> "index.html"

-- Root directory for source HTML templates.
templatePath :: FilePath
templatePath = rootPath </> "template"

-- Source template for the site index page.
templateIndexPath :: FilePath
templateIndexPath = templatePath </> "index.html"

-- Source template for individual post pages.
templatePostPath :: FilePath
templatePostPath = templatePath </> "post.html"

-- Directory containing reusable HTML template components.
templateComponentPath :: FilePath
templateComponentPath = templatePath </> "component"

-- Placeholder syntax used inside templates before component expansion.
--
-- Example result: `<!--<navbar>-->`.
componentPlaceholderPattern :: String
componentPlaceholderPattern = "<!--<" ++ componentPlaceholderToken ++ ">-->"

-- Marker token replaced with a concrete component name when generating a
-- placeholder pattern.
componentPlaceholderToken :: String
componentPlaceholderToken = "name"

-- Public URL root for generated pages.
webRoot :: String
webRoot = "/"

-- Public URL prefix for generated post pages.
webPost :: String
webPost = webRoot ++ "posts"

