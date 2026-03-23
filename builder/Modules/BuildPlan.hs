module Modules.BuildPlan where

import Modules.Post
import Modules.Config

import System.FilePath
import System.Directory
  ( doesFileExist
  , getModificationTime
  )

-- Build steps emitted by the planner.
--
-- `BuildPostPlan` is for one post page, while `BuildIndexPlan` aggregates all
-- posts for rendering the index page.
data BuildPlan 
  = BuildPostPlan PostBuildPlan 
  | BuildIndexPlan IndexBuildPlan

-- Inputs and outputs required to build one post page.
data PostBuildPlan = PostBuildPlan 
  { planPost :: Post
  , planPreprocessedPath :: FilePath
  , planTargetHtmlPath   :: FilePath
  , planPostTemplatePath :: FilePath
  , planPostUrl          :: Url
  } deriving (Show, Eq)

-- Inputs required to build the index page.
data IndexBuildPlan = IndexBuildPlan 
  { planPosts            :: [Post]
  , planIndexTemplatePath :: FilePath
  , planIndexUrl         :: Url
  } deriving (Show, Eq)

-- Constructs a post-page build plan from a parsed source post.
mkBuildPostPlan :: Post -> BuildPlan
mkBuildPostPlan post = BuildPostPlan PostBuildPlan
  { planPost = post
  , planPreprocessedPath = tempPath </> (postName post ++ ".md")
  , planTargetHtmlPath = postPath </> (postName post ++ ".html")
  , planPostTemplatePath = renderedTemplatePostPath
  , planPostUrl = webPostPath ++ postName post ++ ".html"
  }

-- Constructs an index-page build plan from all parsed posts.
mkBuildIndexPlan :: [Post] -> BuildPlan
mkBuildIndexPlan posts = BuildIndexPlan IndexBuildPlan
  { planPosts = posts
  , planIndexTemplatePath = renderedTemplateIndexPath 
  , planIndexUrl = webRoot ++ "index.html"
  }

-- Decides whether a given plan should be rebuilt in this run.
--
-- Current policy:
-- - index is always rebuilt
-- - post page is rebuilt when target does not exist or source is newer
shouldBuild :: BuildPlan -> IO Bool
shouldBuild (BuildIndexPlan _) = return True
shouldBuild (BuildPostPlan plan) = postShouldBuild plan

-- Timestamp-based rebuild decision for one post page.
postShouldBuild :: PostBuildPlan -> IO Bool
postShouldBuild plan = do
  let srcPath = postSourcePath $ planPost plan
  let targetPath = planTargetHtmlPath plan

  targetExists <- doesFileExist $ planTargetHtmlPath plan
  if not targetExists 
    then return True
    else do
      srcTime <- getModificationTime srcPath
      targetTime <- getModificationTime targetPath
      return (srcTime > targetTime)
