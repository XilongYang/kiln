-- | One build's resolved settings, and the plumbing every stage of it
-- shares.
--
-- `BuildEnv` is built once, by `Kiln.Build.kilnBuild`, and threaded
-- through the stages in place of the raw `KilnConfig`: each stage used
-- to re-derive the same handful of values (the cache directory, the
-- webroot, the source directory) out of the nested config for itself,
-- three near-identical copies of the same plumbing. Derived locations
-- and URLs live here as functions of the env, so there's one answer to
-- "where does a post's page go" and "what URL is it published at"
-- rather than one per stage.
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
  ) where

import Control.Exception (IOException, catch, displayException)
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
    -- ^ Scratch space for the duration of one build -- and nothing but
    -- scratch: the templates are read straight from `envTemplateDir`,
    -- which is therefore never written to, as the config's split into
    -- `input` and `target` promises.
    --
    -- Deliberately under `envCacheDir`: that puts it inside the boundary
    -- the config already declares, so @kiln clean@ removes it along with
    -- the rest of the cache and a build interrupted before its own
    -- cleanup leaves it somewhere inspectable -- rather than in a
    -- hardcoded @.temp@ beside the sources, which nothing owned and
    -- which a build wiped on sight, taking any directory of the user's
    -- that happened to share the name.
  , envPost        :: Maybe PostPageConfig
  , envPages       :: [PageConfig]
  , envSearchDb    :: Maybe FilePath
  , envFonts       :: Maybe FontsConfig
  , envTools       :: String
    -- ^ kiln's own version, for the cache fingerprints: a kiln that
    -- renders a post differently than the one that filled the cache
    -- should not then call that post fresh. Free to know -- it is a
    -- compile-time constant.
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

-- | One post's own scratch directory. Per-post rather than shared, so
-- the intermediate files of two posts can't be mistaken for each other
-- (and so rendering them concurrently stays possible).
postWorkDir :: BuildEnv -> String -> FilePath
postWorkDir env slug = envWorkDir env </> "post" </> slug

-- | The file a post's standalone page is written to, or `Nothing` when
-- the site renders no standalone post pages at all.
postOutputPath :: BuildEnv -> String -> Maybe FilePath
postOutputPath env slug =
  (\ppc -> postPageOutputDir ppc </> slug ++ ".html") <$> envPost env

-- | The URL a post is published at, derived from the output directory
-- the config actually names -- or `Nothing` when the site renders no
-- standalone post pages (`target.post` is JSON @null@), because then the
-- post genuinely has nowhere to link to and a template's
-- @$if(posts.url)$@ should say so.
postUrl :: BuildEnv -> String -> Maybe String
postUrl env slug = do
  ppc <- envPost env
  pure (webrootPrefix (envWebroot env) ++ dirPrefix (postPageOutputDir ppc) ++ slug ++ ".html")

-- | The site's webroot as a URL prefix, ending in exactly one @\/@, so
-- that @"\/"@, @"\/blog"@ and @"\/blog\/"@ all prefix a site-relative
-- path the same way. An empty webroot stays empty, leaving links
-- relative.
webrootPrefix :: String -> String
webrootPrefix webroot
  | null webroot = ""
  | otherwise    = dropWhileEnd (== '/') webroot ++ "/"

-- | The site's own origin with no trailing slash, so it joins onto a
-- site-relative path (which `webrootPrefix` has already given a leading
-- one) with exactly one @\/@ between them however the config spelled it.
-- Empty when the site hasn't declared one.
siteRoot :: BuildEnv -> String
siteRoot = dropWhileEnd (== '/') . envSiteUrl

-- | A site-relative path (a `postUrl`, or a URL out of a post's rendered
-- HTML) as an absolute one.
--
-- A site that declared no `Kiln.Config.optSiteUrl` gets `path` back
-- untouched: there is no honest absolute form to produce, and a feed
-- full of root-relative links is at least a feed whose links work for
-- anyone reading it on the site itself.
absoluteUrl :: BuildEnv -> String -> String
absoluteUrl env path
  | null root = path
  | otherwise = root ++ "/" ++ dropWhile (== '/') path
  where
    root = siteRoot env

-- | An output directory as a URL prefix: ending in @\/@, or empty when
-- it names the site root itself, so posts written beside @index.html@
-- are linked as @\/slug.html@ rather than @\/.\/slug.html@.
dirPrefix :: FilePath -> String
dirPrefix dir
  | trimmed `elem` ["", "."] = ""
  | otherwise                = trimmed ++ "/"
  where
    trimmed = dropWhileEnd (== '/') (dropWhile (== '/') dir)

-- | A path under the build's cache directory, from path segments joined
-- as given -- so a cache entry can mirror the directory structure of
-- whatever it tracks (see `Kiln.Build.Page.writePage`) instead of
-- flattening it to a base name two different outputs could collide on.
cachePath :: BuildEnv -> [FilePath] -> FilePath
cachePath env = foldl (</>) (envCacheDir env)

-- | A path under the build's work directory, from path segments joined
-- as given.
workPath :: BuildEnv -> [FilePath] -> FilePath
workPath env = foldl (</>) (envWorkDir env)

-- | Run `act`, prefixing any IO error it raises with `label`.
--
-- Everything the builder shells out to -- pandoc and pyftsubset --
-- reports failure as an `IOException`, and so does a frontmatter that
-- didn't parse. Unlabelled, a hundred-post build surfaces one bad post
-- as a bare @IOError@ naming neither the post nor the step; labelled, it
-- names both.
withContext :: String -> IO a -> IO a
withContext label act = act `catch` rethrow
  where
    rethrow :: IOException -> IO a
    rethrow e = ioError (userError (label ++ ":\n  " ++ displayException e))

-- | Report a step of the build.
--
-- Progress goes to stderr, alongside the warnings and the errors: stdout
-- stays clean for anything a caller might want to pipe, and redirecting
-- it doesn't swallow the part of the output that reports trouble.
logMsg :: String -> IO ()
logMsg = hPutStrLn stderr

-- | Report something the build worked around rather than failed on.
warn :: String -> IO ()
warn = logMsg . ("Warning: " ++)
