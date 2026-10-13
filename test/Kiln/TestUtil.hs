module Kiln.TestUtil (withTempDir, testEnv) where

import Kiln.Build.Env (BuildEnv (..))
import Kiln.Config (PostPageConfig (..), TocConfig (..))
import qualified Kiln.FileTree as FileTree
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

-- | Create a fresh empty directory under the system temp root for the
-- duration of the action, then remove it afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tmpRoot <- getTemporaryDirectory
  (path, h) <- openTempFile tmpRoot "kiln-test-"
  hClose h
  removeFile path
  FileTree.withTempDir path action

-- | A `BuildEnv` for the pure parts of the builder, built directly
-- rather than through `Kiln.Build.Env.mkBuildEnv` -- the latter probes
-- pandoc for its version, which a test of a pure function has no reason
-- to need. Only the webroot and the post page config vary across the
-- tests that use it; everything else is a placeholder.
testEnv :: String -> Maybe PostPageConfig -> BuildEnv
testEnv webroot post =
  BuildEnv
    { envSrcDir = "src"
    , envTemplateDir = "template"
    , envWebroot = webroot
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
