-- | One build's resolved settings, and the plumbing its stages share.
--
-- Built once and threaded through in place of the raw `KilnConfig`, so
-- derived locations and URLs -- where a post's page goes, what URL it is
-- published at -- have one answer here instead of one per stage.
module Kiln.Build.Env
  ( BuildEnv (..)
  , mkBuildEnv
  , postWorkDir
  , postOutputPath
  , postUrl
  , webrootPrefix
  , siteRoot
  , absoluteUrl
  , cachePath
  , workPath
  , withContext
  , logMsg
  , warn
  , mapBounded
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (IOException, bracket_, catch, displayException)
import Data.List (dropWhileEnd)
import Data.Version (showVersion)
import Kiln.Config
  ( FontsConfig
  , InputConfig (..)
  , KilnConfig (..)
  , OptConfig (..)
  , PageConfig
  , PostPageConfig (..)
  , TargetConfig (..)
  , TocConfig
  )
import Paths_kiln (version)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

data BuildEnv = BuildEnv
  { envSrcDir      :: FilePath
  , envTemplateDir :: FilePath
  , envWebroot     :: String
  , envSiteUrl     :: String
  , envToc         :: TocConfig
  , envPrism       :: Bool
  , envMath        :: Bool
  , envCacheDir    :: FilePath
  , envWorkDir     :: FilePath
    -- ^ Scratch space for one build; templates are read straight from
    -- `envTemplateDir`, which is never written to. Under `envCacheDir`
    -- so it stays inside the boundary the config declares: @kiln clean@
    -- covers it, and nothing kiln doesn't own is ever wiped.
  , envPost        :: Maybe PostPageConfig
  , envPages       :: [PageConfig]
  , envSearchDb    :: Maybe FilePath
  , envFonts       :: Maybe FontsConfig
  , envTools       :: String
    -- ^ kiln's own version, part of every cache fingerprint: a kiln that
    -- renders differently must not call an old output fresh.
  }

mkBuildEnv :: KilnConfig -> BuildEnv
mkBuildEnv config =
  BuildEnv
    { envSrcDir      = inputSrcDir input
    , envTemplateDir = inputTemplateDir input
    , envWebroot     = optWebroot opt
    , envSiteUrl     = optSiteUrl opt
    , envToc         = optToc opt
    , envPrism       = optPrism opt
    , envMath        = optMath opt
    , envCacheDir    = cacheDir
    , envWorkDir     = cacheDir </> "work"
    , envPost        = targetPost target
    , envPages       = targetPages target
    , envSearchDb    = targetSearchDb target
    , envFonts       = targetFonts target
    , envTools       = "kiln " ++ showVersion version
    }
  where
    input = configInput config
    opt = configOpt config
    target = configTarget config
    cacheDir = targetCacheDir target

-- | One post's own scratch directory, so two posts' intermediates can't
-- be mistaken for each other.
postWorkDir :: BuildEnv -> String -> FilePath
postWorkDir env slug = envWorkDir env </> "post" </> slug

-- | The file a post's standalone page is written to, or `Nothing` when
-- the site renders none.
postOutputPath :: BuildEnv -> String -> Maybe FilePath
postOutputPath env slug =
  (\ppc -> postPageOutputDir ppc </> slug ++ ".html") <$> envPost env

-- | The URL a post is published at, derived from the output directory
-- the config names. `Nothing` when the site renders no post pages, so a
-- template's @$if(posts.url)$@ sees that there is nowhere to link.
postUrl :: BuildEnv -> String -> Maybe String
postUrl env slug = do
  ppc <- envPost env
  pure (webrootPrefix (envWebroot env) ++ dirPrefix (postPageOutputDir ppc) ++ slug ++ ".html")

-- | The webroot ending in exactly one @\/@, so @"\/"@, @"\/blog"@ and
-- @"\/blog\/"@ prefix a path the same way. Empty stays empty, leaving
-- links relative.
webrootPrefix :: String -> String
webrootPrefix webroot
  | null webroot = ""
  | otherwise    = dropWhileEnd (== '/') webroot ++ "/"

-- | The site's origin with no trailing slash, so it joins onto an
-- already-slash-led path with exactly one @\/@ between them however the
-- config spelled it. Empty when none was declared.
siteRoot :: BuildEnv -> String
siteRoot = dropWhileEnd (== '/') . envSiteUrl

-- | A site-relative path as an absolute URL.
absoluteUrl :: BuildEnv -> String -> String
absoluteUrl env path = siteRoot env ++ "/" ++ dropWhile (== '/') path

-- | An output directory as a URL prefix -- empty at the site root, so
-- posts beside @index.html@ link as @\/slug.html@, not @\/.\/slug.html@.
dirPrefix :: FilePath -> String
dirPrefix dir
  | trimmed `elem` ["", "."] = ""
  | otherwise                = trimmed ++ "/"
  where
    trimmed = dropWhileEnd (== '/') (dropWhile (== '/') dir)

-- | A path under the cache directory, joined as given, so an entry can
-- mirror the structure of what it tracks instead of flattening it to a
-- base name two outputs could collide on.
cachePath :: BuildEnv -> [FilePath] -> FilePath
cachePath env = foldl (</>) (envCacheDir env)

-- | A path under the work directory, joined as given.
workPath :: BuildEnv -> [FilePath] -> FilePath
workPath env = foldl (</>) (envWorkDir env)

-- | Run `act`, prefixing any IO error with `label`: pandoc, pyftsubset
-- and an unparsable frontmatter all fail as a bare @IOError@ naming
-- neither the post nor the step.
withContext :: String -> IO a -> IO a
withContext label act = act `catch` rethrow
  where
    rethrow :: IOException -> IO a
    rethrow e = ioError (userError (label ++ ":\n  " ++ displayException e))

-- | Report a step of the build. Progress shares stderr with the
-- warnings and errors, so stdout stays pipeable and redirecting it
-- doesn't swallow trouble.
logMsg :: String -> IO ()
logMsg = hPutStrLn stderr

-- | Report something the build worked around rather than failed on.
warn :: String -> IO ()
warn = logMsg . ("Warning: " ++)

-- | `mapConcurrently`, capped at the capability count: every task here
-- spawns pandoc or pyftsubset processes of its own, so one task per core
-- is already the machine's worth of work. Order is preserved -- only the
-- log lines come out of it.
mapBounded :: (a -> IO b) -> [a] -> IO [b]
mapBounded act xs = do
  gate <- newQSem =<< getNumCapabilities
  mapConcurrently (bracket_ (waitQSem gate) (signalQSem gate) . act) xs
