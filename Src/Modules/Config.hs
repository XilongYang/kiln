module Modules.Config where

import Modules.ConfigReader (readRootPath)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

-- ---[ Overview ]------------------------------------------------------------
-- | Centralized path and naming configuration for the build pipeline.
--
-- Keeping these constants in one module avoids scattering path construction,
-- cache layout, and template placeholder conventions across the codebase.

-- ---[ Public API ]------------------------------------------------------------

-- | Repository root for all relative build paths.
rootPath :: FilePath
rootPath = unsafePerformIO readRootPath
{-# NOINLINE rootPath #-}

builderPath :: FilePath
builderPath = rootPath </> "Src"

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

-- | Output path for generated search index KLB payload.
searchDBPath :: FilePath
searchDBPath = rootPath </> "searchdb.klb"

-- | Temporary file containing merged charset consumed by @pyftsubset@.
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

-- | Root directory for incremental build cache data.
cacheRoot :: FilePath
cacheRoot = rootPath </> ".cache"

-- | Cache directory for timestamp/state records.
cacheStatePath :: FilePath
cacheStatePath = cacheRoot </> "state"

-- | Build-run state file for global builder-level checks.
builderStatePath :: FilePath
builderStatePath = cacheStatePath </> "builder.state"

-- | State file for expanded post template invalidation.
postTemplateStatePath :: FilePath
postTemplateStatePath = cacheStatePath </> "post-template.state"

-- | State file for expanded index template invalidation.
indexTemplateStatePath :: FilePath
indexTemplateStatePath = cacheStatePath </> "index-template.state"

-- | Directory containing per-post incremental state files.
postStatePath :: FilePath
postStatePath = cacheStatePath </> "post"

-- | Aggregate state file representing metadata-artifact collection.
metaStatePath :: FilePath
metaStatePath = cacheStatePath </> "meta.state"

-- | Root directory for reusable build artifacts generated per page/post.
cacheArtifactsPath :: FilePath
cacheArtifactsPath = cacheRoot </> "artifacts"

-- | Per-post index metadata artifacts (@IndexItem@ KLB files).
metaArtifactsPath :: FilePath
metaArtifactsPath = cacheArtifactsPath </> "meta"

-- | Per-post search record artifacts (@SearchItem@ KLB files).
searchItemArtifactsPath :: FilePath
searchItemArtifactsPath = cacheArtifactsPath </> "search-item"

-- | Per-page charset artifacts used by font subsetting.
charsetArtifactsPath :: FilePath
charsetArtifactsPath = cacheArtifactsPath </> "charset"

-- ---[ Implementation Details ]-----------------------------------------------
-- This module intentionally exposes constants only and keeps no private
-- implementation helpers.
