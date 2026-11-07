{-# LANGUAGE OverloadedStrings #-}

module Kiln.Config
  ( KilnConfig (..)
  , InputConfig (..)
  , OptConfig (..)
  , TargetConfig (..)
  , FontsConfig (..)
  , TocConfig (..)
  , PageConfig (..)
  , PostPageConfig (..)
  , configErrors
  , readConfig
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?), (.!=))
import Data.List (dropWhileEnd, group, intercalate, sort)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeBaseName)

-- | Where kiln reads from -- never written to, never cleaned.
data InputConfig = InputConfig
  { inputSrcDir      :: FilePath
  , inputTemplateDir :: FilePath
  } deriving (Show, Eq)

-- | Site-wide behavior rather than locations. Every field is optional,
-- unlike `TargetConfig`'s: a missing `opt` key is a site with no
-- opinion, a missing `target` key a site that hasn't said where
-- something goes.
data OptConfig = OptConfig
  { optWebroot :: FilePath
  , optSiteUrl :: String
    -- ^ the site's own origin (@"https:\/\/example.com"@), trailing slash
    -- optional. Only a page asking for @absolute-urls@ needs it, and
    -- `configErrors` rejects an empty one once a page does. Defaults to
    -- empty.
  , optToc     :: TocConfig
  , optPrism   :: Bool
    -- ^ whether a fence's language mark is rewritten into the attribute
    -- form Prism's plugins key off of (see
    -- `Kiln.Build.Markdown.rewriteLanguageMarks`). Off for a template
    -- that highlights differently or not at all (@flow@), which then
    -- gets pandoc's own fence handling. Defaults to true, as @default@ loads
    -- Prism.
  , optMath    :: Bool
    -- ^ whether post pages are rendered with pandoc's @--mathjax@. Off
    -- for a template with no math (@flow@). Defaults to true, as @default@ loads
    -- MathJax.
  } deriving (Show, Eq)

-- | Where to write subset fonts, and the font files to subset into it.
-- JSON @null@ for a site with no local fonts, as with `targetPost` --
-- see `Kiln.Build.FontSubset.subsetFonts`.
data FontsConfig = FontsConfig
  { fontsSubsetDir :: FilePath
  , fontsSources   :: [FilePath]
  } deriving (Show, Eq)

-- | One page to generate: its template (under `inputTemplateDir`) and
-- the site-relative path to write it to. `pageOutput` doubles as the
-- page's identity -- two pages sharing one would overwrite each other
-- anyway, so it is already a unique cache key with no second name to
-- keep in sync.
data PageConfig = PageConfig
  { pageTemplate     :: FilePath
  , pageOutput       :: FilePath
  , pageAbsoluteUrls :: Bool
    -- ^ whether this page's @posts@ list arrives with every URL made
    -- absolute against `optSiteUrl` -- the post links and the ones
    -- inside each post's rendered HTML (see
    -- `Kiln.Build.Page.absoluteItems`).
    --
    -- Per page, because it is a property of where the page is read: on
    -- the site a root-relative link is right, in an aggregator the same
    -- link points at the aggregator. One setting, rather than a second
    -- absolute copy of every URL-bearing field. Defaults to false.
  } deriving (Show, Eq)

-- | The standalone page rendered per markdown file, or JSON @null@ for a
-- site that wants none (a flat card-flow feed, say) -- stated
-- explicitly either way, never defaulted. A post with slug @s@ lands at
-- @postPageOutputDir </> s <> ".html"@; `Kiln.Build.Env.postUrl` derives
-- its link from that rather than assuming a directory name.
data PostPageConfig = PostPageConfig
  { postPageTemplate  :: FilePath
  , postPageOutputDir :: FilePath
  } deriving (Show, Eq)

data TocConfig = TocConfig
  { tocEnable  :: Bool
  , tocDepth   :: Int
  , tocOrdered :: Bool
    -- ^ whether the rendered toc's @<ul>@s become @<ol>@s (see
    -- `Kiln.Build.Markdown.olify`). Not a choice a stylesheet could make
    -- for itself: latex.css, which @default@ loads, numbers a nested toc
    -- from counters keyed on @nav ol@, which no @<ul>@ matches however it
    -- is styled. Under
    -- `toc` because that is where someone changing the toc looks.
    -- Defaults to true.
  } deriving (Show, Eq)

-- | Everything kiln generates and `kiln clean` removes.
data TargetConfig = TargetConfig
  { targetSearchDb :: Maybe FilePath
    -- ^ Output path for the search index JSON, or JSON @null@ for a site
    -- with no search to feed -- see
    -- `Kiln.Build.SearchDb.writeSearchDb`.
  , targetCacheDir :: FilePath
  , targetPost     :: Maybe PostPageConfig
  , targetPages    :: [PageConfig]
  , targetFonts    :: Maybe FontsConfig
  } deriving (Show, Eq)

data KilnConfig = KilnConfig
  { configInput  :: InputConfig
  , configOpt    :: OptConfig
  , configTarget :: TargetConfig
  } deriving (Show, Eq)

defaultToc :: TocConfig
defaultToc = TocConfig True 3 True

instance FromJSON InputConfig where
  parseJSON = withObject "input" $ \o ->
    InputConfig
      <$> o .: "src-dir"
      <*> o .: "template-dir"

instance FromJSON OptConfig where
  parseJSON = withObject "opt" $ \o ->
    OptConfig
      <$> o .: "webroot"
      <*> o .:? "site-url" .!= ""
      <*> o .:? "toc" .!= defaultToc
      <*> o .:? "prism" .!= True
      <*> o .:? "math" .!= True

instance FromJSON FontsConfig where
  parseJSON = withObject "fonts" $ \o ->
    FontsConfig
      <$> o .: "subset-dir"
      <*> o .: "sources"

instance FromJSON PageConfig where
  parseJSON = withObject "page" $ \o ->
    PageConfig
      <$> o .: "template"
      <*> o .: "output"
      <*> o .:? "absolute-urls" .!= False

instance FromJSON PostPageConfig where
  parseJSON = withObject "post" $ \o ->
    PostPageConfig
      <$> o .: "template"
      <*> o .: "output-dir"

instance FromJSON TocConfig where
  parseJSON = withObject "toc" $ \o ->
    TocConfig
      <$> o .:? "enable" .!= tocEnable defaultToc
      <*> o .:? "depth" .!= tocDepth defaultToc
      <*> o .:? "ordered" .!= tocOrdered defaultToc

instance FromJSON TargetConfig where
  parseJSON = withObject "target" $ \o ->
    TargetConfig
      <$> o .: "searchdb"
      <*> o .: "cache-dir"
      <*> o .: "post"
      <*> o .: "pages"
      <*> o .: "fonts"

instance FromJSON KilnConfig where
  parseJSON = withObject "kiln-config" $ \o ->
    KilnConfig
      <$> o .: "input"
      <*> o .: "opt"
      <*> o .: "target"

configFileName :: FilePath
configFileName = "kiln-config.json"

-- | Everything wrong with a parsed config, one message per problem.
configErrors :: KilnConfig -> [String]
configErrors cfg =
  [ "\"" ++ pageOutput page ++ "\" asks for \"absolute-urls\", but opt.\"site-url\" is empty"
  | null (dropWhileEnd (== '/') (optSiteUrl (configOpt cfg)))
  , page <- targetPages (configTarget cfg)
  , pageAbsoluteUrls page
  ]
    -- A subset is named after its source's base name, so two sources
    -- sharing one would be two pyftsubset runs over a single output.
    ++ [ "more than one font source is named " ++ name ++ "; they would all subset to " ++ name ++ ".woff2"
       | name <- repeated (map takeBaseName fontSources)
       ]
  where
    fontSources = maybe [] fontsSources (targetFonts (configTarget cfg))
    repeated names = [name | name : _ : _ <- group (sort names)]

readConfig :: IO KilnConfig
readConfig = do
  exists <- doesFileExist configFileName
  unless exists $ die missingErrMsg
  result <- eitherDecodeFileStrict configFileName
  cfg <- either (die . parseErrMsg) pure result
  case configErrors cfg of
    []   -> pure cfg
    errs -> die (intercalate "\n" (map prefixed errs))
  where
    missingErrMsg = configFileName ++ " not found"
    parseErrMsg err = "Failed to parse " ++ configFileName ++ ": " ++ err
    prefixed err = configFileName ++ ": " ++ err
