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
  { planPost             :: Post
  , planPreprocessedPath :: FilePath
  , planBuiltHtmlPath :: FilePath
  , planTargetHtmlPath   :: FilePath
  , planPostTemplatePath :: FilePath
  } deriving (Show, Eq)

-- | Concrete plan payload for building the homepage index.
data IndexBuildPlan = IndexBuildPlan 
  { planIndexItems        :: [IndexItem]
  , planIndexHtmlPath     :: FilePath
  , planIndexTemplatePath :: FilePath
  , planIndexUrl          :: Url
  } deriving (Show, Eq)

-- | Creates a post build plan from one parsed post.
mkBuildPostPlan :: Post -> BuildPlan
mkBuildPostPlan post = BuildPostPlan PostBuildPlan
  { planPost = post
  , planPreprocessedPath = tempPath </> (postName post ++ ".md")
  , planBuiltHtmlPath = tempPath </> (postName post ++ ".html")
  , planTargetHtmlPath = postPath </> (postName post ++ ".html")
  , planPostTemplatePath = renderedTemplatePostPath
  }

-- | Creates an index build plan from all parsed posts.
mkBuildIndexPlan :: [Post] -> BuildPlan
mkBuildIndexPlan posts = BuildIndexPlan IndexBuildPlan
  { planIndexItems = map mkIndexItem posts
  , planIndexHtmlPath = indexPath
  , planIndexTemplatePath = renderedTemplateIndexPath 
  , planIndexUrl = webRoot ++ "index.html"
  }

