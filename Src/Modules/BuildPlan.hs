module Modules.BuildPlan 
  ( BuildPlan (..)
  , PostBuildPlan (..)
  , IndexBuildPlan (..)
  , mkBuildPostPlan
  , mkBuildIndexPlan) where

import Modules.Config
import Modules.Index.Item
import Modules.Post
import Modules.TypeAlias
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Typed build-plan definitions for the static-site build pipeline.
--
-- This module only models "what to build" and "where each output lives".
-- Rebuild decisions are handled separately in 'Modules.BuildJudger'.

-- ---[ Public API ]------------------------------------------------------------

-- | Sum type for all supported build actions.
data BuildPlan 
  = BuildPostPlan PostBuildPlan 
  | BuildIndexPlan IndexBuildPlan

-- | Concrete plan payload for building one post page and its cache artifacts.
data PostBuildPlan = PostBuildPlan 
  { planPostSourcePath :: FilePath
    -- ^ Source markdown path under @src/@.
  , planPreprocessedPath :: FilePath
    -- ^ Intermediate markdown (after custom preprocessing).
  , planBuiltHtmlPath :: FilePath
    -- ^ Temporary HTML produced directly by pandoc.
  , planTargetHtmlPath :: FilePath
    -- ^ Final shipped post HTML path under @post/@.
  , planPostTemplatePath :: FilePath
    -- ^ Expanded post template file used by pandoc.
  , planPostStatePath :: FilePath
    -- ^ Incremental state record path for this post.
  , planPostMetaPath :: FilePath
    -- ^ Per-post index metadata artifact (@IndexItem@ KLB).
  , planPostSearchItemPath :: FilePath
    -- ^ Per-post search artifact (@SearchItem@ KLB).
  , planPostCharsetPath :: FilePath
    -- ^ Per-post charset artifact for font subsetting.
  , planPostUrl :: Url
    -- ^ Canonical public URL of the generated post.
  } deriving (Show, Eq)

-- | Concrete plan payload for building the homepage index.
data IndexBuildPlan = IndexBuildPlan 
  { planIndexHtmlPath :: FilePath
    -- ^ Final output path for @index.html@.
  , planIndexTemplatePath :: FilePath
    -- ^ Expanded index template file path.
  , planIndexUrl :: Url
    -- ^ Canonical public URL of the generated index page.
  } deriving (Show, Eq)

-- | Builds a deterministic post plan from one source markdown path.
mkBuildPostPlan :: FilePath -> BuildPlan
mkBuildPostPlan path = BuildPostPlan PostBuildPlan
  { planPostSourcePath = path
  , planPreprocessedPath = tempPath </> (baseName ++ ".md")
  , planBuiltHtmlPath = tempPath </> (baseName ++ ".html")
  , planTargetHtmlPath = postPath </> (baseName ++ ".html")
  , planPostTemplatePath = renderedTemplatePostPath
  , planPostStatePath = postStatePath </> (baseName ++ ".state")
  , planPostMetaPath = metaArtifactsPath </> (baseName ++ ".klb")
  , planPostSearchItemPath = searchItemArtifactsPath </> (baseName ++ ".klb")
  , planPostCharsetPath = charsetArtifactsPath </> (baseName ++ ".txt")
  , planPostUrl = webPostPath ++ baseName ++ ".html"
  }
  where
    baseName = takeBaseName path

-- | Builds the canonical index plan.
mkBuildIndexPlan :: BuildPlan
mkBuildIndexPlan = BuildIndexPlan IndexBuildPlan
  { planIndexHtmlPath = indexPath
  , planIndexTemplatePath = renderedTemplateIndexPath 
  , planIndexUrl = webRoot ++ "index.html"
  }
