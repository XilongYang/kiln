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
  , readConfig
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.:), (.:?), (.!=))
import System.Directory (doesFileExist)
import System.Exit (die)

-- | The read-only directories kiln finds its own source material in --
-- never written to, never cleaned.
data InputConfig = InputConfig
  { inputSrcDir      :: FilePath
  , inputTemplateDir :: FilePath
  } deriving (Show, Eq)

-- | Site-wide behavior, as opposed to filesystem locations. Every field
-- here is optional with a default, unlike `TargetConfig`'s: a missing
-- `opt` key is a site that hasn't got an opinion, whereas a missing
-- `target` key is a site that hasn't said where something goes.
data OptConfig = OptConfig
  { optWebroot :: FilePath
  , optSiteUrl :: String
    -- ^ the site's own origin (@"https:\/\/example.com"@), with no
    -- trailing slash required -- the one thing a site built entirely
    -- from `optWebroot`-relative links never had to know, and the one
    -- thing a feed cannot do without: an entry's @\<link\>@ and @\<id\>@
    -- are read on someone else's domain, where a root-relative path
    -- resolves against the reader, not the site. Defaults to empty,
    -- which leaves every URL exactly as relative as it is today -- so a
    -- site generating no feed never has to state it.
  , optToc     :: TocConfig
  , optPrism   :: Bool
    -- ^ whether a fenced code block's language mark is rewritten into
    -- the pandoc attribute form Prism's line-numbers/match-braces
    -- plugins key off of (see
    -- `Kiln.Build.Markdown.rewriteLanguageMarks`). Defaults to true,
    -- matching the bundled @default@ template, which loads Prism; a
    -- template that highlights differently, or not at all (like
    -- @flow@), turns it off and gets pandoc's own fence handling.
  , optMath    :: Bool
    -- ^ whether post pages are rendered with pandoc's @--mathjax@.
    -- Defaults to true, matching the bundled @default@ template, which
    -- loads MathJax; a template with no math (like @flow@) turns it off.
  } deriving (Show, Eq)

-- | Where to write subset fonts (`fontsSubsetDir`), and the font files
-- (full paths) to subset into it. A site with no local fonts to subset
-- states this as JSON @null@ instead, same as `targetPost` -- see
-- `Kiln.Build.FontSubset.subsetFonts`.
data FontsConfig = FontsConfig
  { fontsSubsetDir :: FilePath
  , fontsSources   :: [FilePath]
  } deriving (Show, Eq)

-- | One page to generate: its own template (a path under
-- `inputTemplateDir`), and the site-relative path to write it to. A
-- page's `pageOutput` doubles as its own identity -- two pages can't
-- share one without one silently overwriting the other's output
-- regardless of caching, so it's already a safe, unique key for the
-- page's own build cache; there's no separate name to keep in sync.
data PageConfig = PageConfig
  { pageTemplate     :: FilePath
  , pageOutput       :: FilePath
  , pageAbsoluteUrls :: Bool
    -- ^ whether this page's @posts@ list is handed to the template with
    -- every URL made absolute against `optSiteUrl` -- the post links
    -- themselves, and the ones inside each post's rendered HTML (see
    -- `Kiln.Build.Page.absoluteItems`).
    --
    -- A property of the page rather than of the content, because it's a
    -- property of where the page is read: an HTML page is read on the
    -- site, where a root-relative link is exactly right, while a feed is
    -- read inside an aggregator, where the same link points at the
    -- aggregator. So this is one setting per page instead of a second,
    -- absolute copy of every URL-bearing field for every template to
    -- choose between. Defaults to false.
  } deriving (Show, Eq)

-- | The per-post standalone page @kiln build@ renders for every
-- markdown file in `inputSrcDir`, if any -- a site like a flat
-- card-flow feed may not want one at all (see `Kiln.Config.readConfig`'s
-- caller, which requires this to be stated explicitly as either this or
-- JSON @null@, never silently defaulted). `postPageOutputDir` is a
-- directory; a post with slug @s@ is written to
-- @postPageOutputDir </> s <> ".html"@, and published at the matching
-- URL -- see `Kiln.Build.Env.postUrl`, which derives the link from this
-- rather than assuming a directory name.
data PostPageConfig = PostPageConfig
  { postPageTemplate  :: FilePath
  , postPageOutputDir :: FilePath
  } deriving (Show, Eq)

data TocConfig = TocConfig
  { tocEnable  :: Bool
  , tocDepth   :: Int
  , tocOrdered :: Bool
    -- ^ whether the rendered table of contents' @<ul>@s are rewritten
    -- into @<ol>@s (see `Kiln.Build.Markdown.olify`).
    --
    -- Not a styling choice a stylesheet could make for itself: latex.css,
    -- which the bundled @default@ template loads, numbers a nested toc
    -- from CSS counters keyed on @nav ol@, and those rules don't match a
    -- @<ul>@ at all however it's styled. So this is the same kind of
    -- setting as `optPrism` and `optMath` -- the markup shape some
    -- library the template loads needs -- and it lives under `toc`
    -- because that's where someone changing the table of contents looks,
    -- whether or not a given field happens to reach pandoc as an
    -- argument. Defaults to true, matching @default@.
  } deriving (Show, Eq)

-- | Everything kiln generates and `kiln clean` removes.
data TargetConfig = TargetConfig
  { targetSearchDb :: Maybe FilePath
    -- ^ Output path for the generated search index JSON, or JSON @null@
    -- if the site has no search feature to feed -- same convention as
    -- `targetPost`/`targetFonts`. See `Kiln.Build.SearchDb.writeSearchDb`.
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

readConfig :: IO KilnConfig
readConfig = do
  exists <- doesFileExist configFileName
  unless exists $ die missingErrMsg
  result <- eitherDecodeFileStrict configFileName
  either (die . parseErrMsg) pure result
  where
    missingErrMsg = configFileName ++ " not found"
    parseErrMsg err = "Failed to parse " ++ configFileName ++ ": " ++ err
