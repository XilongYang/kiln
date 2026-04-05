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
-- | Typed build-plan definitions and rebuild decision rules.
--
-- This module encodes post/index build intents as data and provides plan
-- constructors plus incremental-build checks.

-- ---[ Public API ]------------------------------------------------------------

-- | Sum type for all supported build actions.
data BuildPlan 
  = BuildPostPlan PostBuildPlan 
  | BuildIndexPlan IndexBuildPlan

-- | Concrete plan payload for building one post page.
data PostBuildPlan = PostBuildPlan 
  { planPostSourcePath   :: FilePath
  , planPreprocessedPath :: FilePath
  , planBuiltHtmlPath    :: FilePath
  , planTargetHtmlPath   :: FilePath
  , planPostTemplatePath :: FilePath
  , planPostUrl          :: Url
  } deriving (Show, Eq)

-- | Concrete plan payload for building the homepage index.
data IndexBuildPlan = IndexBuildPlan 
  { planIndexHtmlPath     :: FilePath
  , planIndexTemplatePath :: FilePath
  , planIndexUrl          :: Url
  } deriving (Show, Eq)

-- | Creates a post build plan from one parsed post.
mkBuildPostPlan :: FilePath -> BuildPlan
mkBuildPostPlan path = BuildPostPlan PostBuildPlan
  { planPostSourcePath = path
  , planPreprocessedPath = tempPath </> (baseName ++ ".md")
  , planBuiltHtmlPath = tempPath </> (baseName ++ ".html")
  , planTargetHtmlPath = postPath </> (baseName ++ ".html")
  , planPostTemplatePath = renderedTemplatePostPath
  , planPostUrl = webPostPath ++ baseName ++ ".html"
  }
  where
    baseName = takeBaseName path

-- | Creates an index build plan from all parsed posts.
mkBuildIndexPlan :: BuildPlan
mkBuildIndexPlan = BuildIndexPlan IndexBuildPlan
  { planIndexHtmlPath = indexPath
  , planIndexTemplatePath = renderedTemplateIndexPath 
  , planIndexUrl = webRoot ++ "index.html"
  }

