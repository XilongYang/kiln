module Modules.Config where

import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Centralized path and naming configuration for the build pipeline.
--
-- Keeping these constants in one module avoids scattering path construction
-- and placeholder conventions across builders, template logic, and UT.

-- ---[ Configuration ]------------------------------------------------------------

-- | Repository root for all relative build paths.
rootPath :: FilePath
rootPath = "."

builderPath :: FilePath
builderPath = rootPath </> "builder"

-- | Directory containing source markdown posts.
srcPath :: FilePath
srcPath = rootPath </> "src"

-- | Output directory for generated post pages.
postPath :: FilePath
postPath = rootPath </> "post"

-- | Output path for the generated site index page.
indexPath :: FilePath
indexPath = rootPath </> "index.html"

-- | Scratch directory for intermediate build artifacts.
tempPath :: FilePath
tempPath = rootPath </> "temp"

-- | Temporary rendered post template path.
renderedTemplatePostPath :: FilePath
renderedTemplatePostPath = tempPath </> "post.html"

-- | Temporary rendered index template path.
renderedTemplateIndexPath :: FilePath
renderedTemplateIndexPath = tempPath </> "index.html"

-- | Root directory for source HTML templates.
templatePath :: FilePath
templatePath = rootPath </> "template"

-- | Source template for the site index page.
templateIndexPath :: FilePath
templateIndexPath = templatePath </> "index.html"

-- | Source template for individual post pages.
templatePostPath :: FilePath
templatePostPath = templatePath </> "post.html"

-- | Directory containing reusable HTML template components.
templateComponentPath :: FilePath
templateComponentPath = templatePath </> "component"

-- | Placeholder syntax used before component expansion.
--
-- Example output: @<!--<navbar>-->@.
componentPlaceholderPattern :: String
componentPlaceholderPattern = "<!--<" ++ componentPlaceholderToken ++ ">-->"

-- | Marker token replaced with concrete component names in
-- 'componentPlaceholderPattern'.
componentPlaceholderToken :: String
componentPlaceholderToken = "name"

-- | Public URL root for generated pages.
webRoot :: String
webRoot = "/"

-- | Public URL prefix for generated post pages.
webPostPath :: String
webPostPath = webRoot ++ "post/"

-- | Output path for generated search index JSON.
searchDBPath :: FilePath
searchDBPath = rootPath </> "searchdb.json"

-- | Temporary file containing deduplicated charset used for font subsetting.
fontSetPath :: FilePath
fontSetPath = tempPath </> "fontset.txt"

-- | Font asset directory.
fontPath :: FilePath
fontPath = rootPath  </> "res" </> "fonts"

-- | Source font file path used as subsetting input.
originFontFilePath :: FilePath
originFontFilePath =  fontPath </> "SourceHanSerifCN-Regular.otf"

-- | Generated subset font output path.
subsetFontFilePath :: FilePath
subsetFontFilePath = fontPath </> "SourceHanSerifCN-Subset.woff2"

-- Root directory for incremental build cache artifacts.
cacheRoot :: FilePath
cacheRoot = rootPath </> ".cache"

cacheStatePath :: FilePath
cacheStatePath = cacheRoot </> "state"

cacheBuilderPath :: FilePath
cacheBuilderPath = cacheStatePath </> "builder.cache"

cachePostTemplatePath :: FilePath
cachePostTemplatePath  = cacheStatePath </> "post-template.cache"

cacheIndexTemplatePath :: FilePath
cacheIndexTemplatePath  = cacheStatePath </> "index-template.cache"

