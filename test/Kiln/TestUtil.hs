module Kiln.TestUtil (withTempDir, testEnv) where

import Kiln.Build.Env (BuildEnv (..))
import Kiln.Config (PostPageConfig (..), TocConfig (..))
import qualified Kiln.FileTree as FileTree
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

-- | A fresh empty directory under the system temp root, removed
-- afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tmpRoot <- getTemporaryDirectory
  (path, h) <- openTempFile tmpRoot "kiln-test-"
  hClose h
  removeFile path
  FileTree.withTempDir path action

-- | A `BuildEnv` built directly rather than through
-- `Kiln.Build.Env.mkBuildEnv`, which would want a whole
-- `Kiln.Config.KilnConfig` a pure test has no reason to assemble. Only
-- the webroot and post page config vary; a test needing more overrides
-- by record update.
testEnv :: String -> Maybe PostPageConfig -> BuildEnv
testEnv webroot post =
  BuildEnv
    { envSrcDir = "src"
    , envTemplateDir = "template"
    , envWebroot = webroot
    , envSiteUrl = ""
    , envToc = TocConfig True 3 True
    , envPrism = True
    , envMath = True
    , envCacheDir = ".cache"
    , envWorkDir = ".cache/work"
    , envPost = post
    , envPages = []
    , envSearchDb = Nothing
    , envFonts = Nothing
    , envTools = "test-tools"
    }
