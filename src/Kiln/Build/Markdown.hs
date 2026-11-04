-- | The pure transforms the builder applies to a post before pandoc, and
-- to pandoc's output afterwards -- the toc (`olify`) and a post body's
-- URLs (`absolutizeUrls`).
--
-- Apart from the rendering because nearly every edge case lives here --
-- fenced code blocks, YAML frontmatter, the @\<!--more--\>@ marker -- and
-- pure functions can be tested without pandoc on the @PATH@.
module Kiln.Build.Markdown
  ( FenceCtx (..)
  , scanFences
  , splitAbstract
  , splitFrontmatter
  , rewriteLanguageMarks
  , olify
  , absolutizeUrls
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Kiln.Str (replaceAll, trim)

-- | Where a line sits relative to the fenced code blocks around it.
data FenceCtx
  = Prose
    -- ^ outside every fence
  | Opener String String String
    -- ^ a fence's opening delimiter, split into its indent, its run of
    -- backticks, and its info string (the language mark, trimmed)
  | InFence
    -- ^ inside a fence -- its body, and its closing delimiter
  deriving (Show, Eq)

-- | Pair every line of a post with its `FenceCtx`.
--
-- A fence of @n@ backticks closes only on @n@ or more, so a fence nested
-- in a longer one is just text -- which is what keeps
-- `rewriteLanguageMarks` and `splitAbstract` off markdown a post is
-- merely quoting.
scanFences :: [String] -> [(String, FenceCtx)]
scanFences = go Nothing
  where
    go _ [] = []
    go (Just open) (l : ls)
      | isCloser open l = (l, InFence) : go Nothing ls
      | otherwise       = (l, InFence) : go (Just open) ls
    go Nothing (l : ls) = case fenceOpener l of
      Just (indent, ticks, info) ->
        (l, Opener indent ticks info) : go (Just (length ticks)) ls
      Nothing -> (l, Prose) : go Nothing ls

-- | A line's indent, backtick run and info string, if it opens a fence:
-- three or more backticks and, as CommonMark requires, none in the info
-- string.
fenceOpener :: String -> Maybe (String, String, String)
fenceOpener line
  | length ticks >= 3, '`' `notElem` rest = Just (indent, ticks, trim rest)
  | otherwise                             = Nothing
  where
    (indent, stripped) = span isSpace line
    (ticks, rest)      = span (== '`') stripped

-- | Whether a line closes a fence opened with `open` backticks: a run of
-- at least that many, with nothing but whitespace around it.
isCloser :: Int -> String -> Bool
isCloser open line = length ticks >= open && all isSpace rest
  where
    (ticks, rest) = span (== '`') (dropWhile isSpace line)

-- | Rewrite a fence's language mark into the pandoc attribute form
-- Prism's line-numbers\/match-braces plugins key off of.
--
-- Only a real opener with a bare language name. An info string already
-- carrying pandoc attributes would become a nested attribute block,
-- which pandoc renders as a paragraph of inline code -- losing the whole
-- block.
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks = unlines . map rewrite . scanFences . lines
  where
    rewrite (line, Opener indent ticks info)
      | isPlainLanguageMark info = indent ++ ticks ++ attrs info
      | otherwise                = line
    rewrite (line, _) = line
    attrs info = replaceAll "[mark]" info " {.language-[mark] .line-numbers .match-braces}"

-- | Whether an info string is a bare language name, and so safe to
-- splice into a pandoc class. Anything else is left for pandoc to read
-- as the post wrote it.
isPlainLanguageMark :: String -> Bool
isPlainLanguageMark info = not (null info) && all ok info
  where
    ok c = isAlphaNum c || c `elem` "-_+#"

-- | Split a post on a line holding only the @\<!--more--\>@ marker: what
-- precedes it is the abstract, what follows is the body -- reattached to
-- any YAML frontmatter, so pandoc still sees the post's metadata. No
-- marker means no abstract and an unchanged source.
--
-- Only a marker in prose counts; one inside a fence is code the post is
-- showing.
splitAbstract :: String -> (Maybe String, String)
splitAbstract src = case break isMoreMarker (scanFences rest) of
  (before, _ : after) ->
    ( Just (trim (unlines (map fst before)))
    , unlines (frontmatter ++ "" : map fst after)
    )
  (_, []) -> (Nothing, src)
  where
    (frontmatter, rest) = splitFrontmatter (lines src)
    isMoreMarker (l, ctx) = ctx == Prose && trim l == "<!--more-->"

-- | Split off a leading @---@-delimited YAML block, delimiters
-- included.
splitFrontmatter :: [String] -> ([String], [String])
splitFrontmatter (l : ls)
  | trim l == "---" = case break ((== "---") . trim) ls of
      (fm, closing : rest) -> (l : fm ++ [closing], rest)
      (_, [])              -> ([], l : ls)
splitFrontmatter ls = ([], ls)

-- | Rewrite a rendered toc's @<ul>@s into @<ol>@s, for the numbered toc
-- `Kiln.Config.tocOrdered` asks for: pandoc's @--toc@ only emits
-- @<ul>@s.
olify :: String -> String
olify = replaceAll "</ul>" "</ol>" . replaceAll "<ul>" "<ol>"

-- | The attributes whose value is a URL, and so the only ones
-- `absolutizeUrls` rewrites.
--
-- A short list rather than "any value starting with a slash": an @alt@
-- or @title@ that begins with one is prose. @srcset@ is absent because
-- its comma-and-descriptor syntax holds several URLs and needs parsing
-- of its own, so a hand-written one stays verbatim.
urlAttrs :: [String]
urlAttrs = ["href", "src", "poster"]

-- | Rewrite the URLs in a post's rendered HTML so they still resolve
-- away from the page they were rendered for -- in a feed reader, which
-- resolves a relative URL against its own domain.
--
-- `absolutize` makes a site-relative path absolute (see
-- `Kiln.Build.Env.absoluteUrl`); `pageUrl` is the post's own page, or
-- `Nothing` for a site that renders none. Each kind of URL needs a
-- different base:
--
-- * root-relative (@\/res\/img.png@) -- the site's origin;
-- * a bare fragment (@#fn1@, every footnote pandoc generates) -- the
--   post's own page, there being no such anchor in the reader;
-- * document-relative (@..\/res\/img.png@) -- the post page's
--   directory, dot segments and all;
-- * absolute or protocol-relative -- left as written.
--
-- A URL whose base can't be known is left alone rather than guessed at:
-- it still points where it always did.
absolutizeUrls :: (String -> String) -> Maybe String -> String -> String
absolutizeUrls absolutize pageUrl = betweenTags
  where
    -- Inside a tag and nowhere else: a post quoting HTML contains
    -- `src="/res/a.png"` as text, with no tag around it -- in a code
    -- block not even a literal `<`, pandoc having escaped it.
    betweenTags [] = []
    betweenTags ('<' : c : cs)
      | isAlpha c || c == '/' = '<' : c : withinTag cs
    betweenTags (c : cs) = c : betweenTags cs

    withinTag [] = []
    withinTag ('>' : cs) = '>' : betweenTags cs
    -- After whitespace, so the match is a whole attribute name: the
    -- `href` inside `data-href="..."` is not one.
    withinTag (c : cs)
      | isSpace c, Just (name, quote, value, rest) <- urlAttrAt cs =
          c : name ++ '=' : quote : resolve value ++ quote : withinTag rest
      | otherwise = c : withinTag cs

    resolve value
      | null value                 = value
      | "//" `isPrefixOf` value    = value
      | hasScheme value            = value
      | "/" `isPrefixOf` value     = removeDotSegments (absolutize value)
      | "#" `isPrefixOf` value     = maybe value (++ value) pageUrl
      | otherwise                  = maybe value (resolveAgainstPage value) pageUrl

    resolveAgainstPage value page = removeDotSegments (parentUrl page ++ value)

-- | The `urlAttrs` attribute at the head of `s`, if one is there with a
-- quoted value: its name, quote character, value and the rest.
urlAttrAt :: String -> Maybe (String, Char, String, String)
urlAttrAt s = foldr attempt Nothing urlAttrs
  where
    attempt name fallback = case stripPrefix (name ++ "=") s of
      Just (quote : afterQuote)
        | quote == '"' || quote == '\'' -> case break (== quote) afterQuote of
            (value, _ : rest) -> Just (name, quote, value, rest)
            -- An unterminated attribute isn't something to rewrite half of.
            (_, [])           -> Nothing
      _ -> fallback

-- | Whether a URL carries a scheme (@https:@, @mailto:@) and so is
-- already absolute. The colon must precede any @\/@, @?@ or @#@, else
-- @img\/a:b.png@ would read as one.
hasScheme :: String -> Bool
hasScheme url = case break (== ':') url of
  (scheme@(c : _), ':' : _) -> isAlpha c && all schemeChar scheme
  _                         -> False

-- | Whether a character can appear in a URL scheme.
schemeChar :: Char -> Bool
schemeChar c = isAlphaNum c || c `elem` "+-."

-- | The directory a URL sits in, trailing @\/@ included. Empty for a URL
-- with no @\/@, which leaves it untouched.
parentUrl :: String -> String
parentUrl = reverse . dropWhile (/= '/') . reverse

-- | Collapse a URL's @.@ and @..@ segments, so a joined URL reads as
-- @\/res\/img.svg@ rather than @\/post\/..\/res\/img.svg@: a browser
-- normalizes before asking, but not everything that reads a feed does.
--
-- A @..@ with nothing left to climb is dropped, the site root having no
-- parent, and the authority is never climbed into -- so no URL can
-- normalize onto a different host.
removeDotSegments :: String -> String
removeDotSegments url = origin ++ intercalate "/" (reverse (foldl step [] (splitSlashes path)))
  where
    (origin, path) = splitOrigin url
    step stack "." = stack
    step stack ".." = case stack of
      "" : _      -> stack  -- already at the root
      _ : climbed -> climbed
      []          -> []
    step stack segment = segment : stack

-- | A URL's @scheme:\/\/authority@, split from its path. Empty origin for
-- a URL that is only a path.
splitOrigin :: String -> (String, String)
splitOrigin url = case break (== ':') url of
  (scheme@(c : _), ':' : '/' : '/' : rest)
    | isAlpha c, all schemeChar scheme ->
        let (authority, path) = break (== '/') rest
         in (scheme ++ "://" ++ authority, path)
  _ -> ("", url)

-- | Split a URL path on @\/@, keeping the empty segments that record a
-- leading or trailing slash.
splitSlashes :: String -> [String]
splitSlashes path = case break (== '/') path of
  (segment, '/' : rest) -> segment : splitSlashes rest
  (segment, _)          -> [segment]
