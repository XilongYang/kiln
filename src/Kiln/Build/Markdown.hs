-- | The pure source transforms the builder applies to a post before
-- handing it to pandoc, and the ones it applies to pandoc's rendered
-- output afterwards -- the table of contents (`olify`, for
-- `Kiln.Build.Post`) and a post body's URLs (`absolutizeUrls`, for
-- `Kiln.Build.Page`).
--
-- Kept apart from the rendering itself because this is where nearly all
-- of the builder's edge cases live -- fenced code blocks, YAML
-- frontmatter, the @\<!--more--\>@ marker -- and as pure functions they
-- can be tested exhaustively without pandoc on the @PATH@.
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
-- A fence opened with @n@ backticks only closes on a line of @n@ or more
-- backticks and nothing else, so a fence nested inside a longer one is
-- just text. That's what keeps `rewriteLanguageMarks` and `splitAbstract`
-- from rewriting or splitting markdown a post is merely quoting -- a post
-- *about* markdown is exactly the kind that would otherwise be mangled.
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
-- at least three backticks, and -- as CommonMark requires of a backtick
-- fence -- no backtick anywhere in the info string that follows.
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

-- | Rewrite a fenced code block's language mark into the pandoc
-- attribute form that gives Prism's line-numbers/match-braces plugins
-- something to key off of.
--
-- Only a real fence opener carrying a bare language name is touched.
-- Everything else is passed through verbatim: a fence with no language
-- mark, a fence whose info string already carries its own pandoc
-- attributes (@```python {.numberLines}@ -- splicing into that produced
-- a nested attribute block that pandoc silently rendered as a paragraph
-- of inline code, losing the whole block), and every line inside a
-- fence.
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks = unlines . map rewrite . scanFences . lines
  where
    rewrite (line, Opener indent ticks info)
      | isPlainLanguageMark info = indent ++ ticks ++ attrs info
      | otherwise                = line
    rewrite (line, _) = line
    attrs info = replaceAll "[mark]" info " {.language-[mark] .line-numbers .match-braces}"

-- | Whether an info string is a bare language name safe to splice into a
-- pandoc class. Anything else -- empty, an attribute block of its own,
-- several words, or a name carrying characters a class can't hold -- is
-- left for pandoc to interpret exactly as the post wrote it.
isPlainLanguageMark :: String -> Bool
isPlainLanguageMark info = not (null info) && all ok info
  where
    ok c = isAlphaNum c || c `elem` "-_+#"

-- | Split a post's source on a line consisting solely of the
-- @\<!--more--\>@ marker. The text before the marker becomes the post's
-- abstract; the text after it (reattached to any YAML frontmatter, so
-- pandoc still sees the post's title\/date\/etc.) becomes the body that
-- gets rendered. Posts without the marker are passed through unchanged,
-- with no abstract.
--
-- Only a marker in prose counts: one inside a fenced code block is part
-- of the code a post is showing, not an instruction to the builder.
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

-- | Split off a leading YAML metadata block (the @---@-delimited lines,
-- delimiters included) from the rest of a post's lines, if one is
-- present.
splitFrontmatter :: [String] -> ([String], [String])
splitFrontmatter (l : ls)
  | trim l == "---" = case break ((== "---") . trim) ls of
      (fm, closing : rest) -> (l : fm ++ [closing], rest)
      (_, [])              -> ([], l : ls)
splitFrontmatter ls = ([], ls)

-- | Rewrite a rendered toc fragment's @<ul>@\/@</ul>@ tags into
-- @<ol>@\/@</ol>@, so the toc reads as a numbered list. Applied only
-- when the site asks for it (`Kiln.Config.tocOrdered`); pandoc's @--toc@
-- only ever emits @<ul>@s.
olify :: String -> String
olify = replaceAll "</ul>" "</ol>" . replaceAll "<ul>" "<ol>"

-- | The attributes of rendered post HTML whose value is a URL, and so
-- the only ones `absolutizeUrls` rewrites.
--
-- A deliberately short list rather than "every attribute whose value
-- starts with a slash": an @alt@ or @title@ that happens to begin with
-- one is prose, and rewriting it would corrupt text the reader displays.
-- @srcset@ is absent because kiln never generates one -- its
-- comma-and-descriptor syntax holds several URLs at once and needs
-- parsing of its own, so a post that hand-writes one keeps it verbatim.
urlAttrs :: [String]
urlAttrs = ["href", "src", "poster"]

-- | Rewrite the URLs in a post's rendered HTML so they still resolve
-- when the HTML is read somewhere other than the page it was rendered
-- for -- inside a feed reader, which resolves a relative URL against
-- its own domain, not the site's.
--
-- `absolutize` turns a site-relative path into an absolute URL (see
-- `Kiln.Build.Env.absoluteUrl`), and `pageUrl` is the absolute URL of
-- the post's own page, or `Nothing` for a site that renders none.
--
-- Four kinds of URL turn up in a rendered post, and each needs
-- something different:
--
-- * root-relative (@\/res\/img.png@, what a post written for a site
--   with a @\/@ webroot contains) -- joined onto the site's origin;
-- * a bare fragment (@#fn1@, which is every footnote reference and
--   back-link pandoc generates) -- resolved against the post's own
--   page, since in a reader there is no such anchor on the page the
--   content is being displayed in;
-- * document-relative (@..\/res\/img.png@, which is what a post that
--   links a file by relative path gets, pandoc having passed the path
--   through as written, and what a site with a relative webroot
--   contains throughout) -- resolved against the post page's own
--   directory, dot segments and all;
-- * already absolute (@https:\/\/...@) or protocol-relative
--   (@\/\/cdn...@) -- left exactly as written.
--
-- A URL whose base can't be known is left alone rather than guessed at:
-- a broken link in a feed is better than a confidently wrong one, and
-- it still points where it always did. Nothing outside a tag is touched
-- at all, so a post that quotes HTML keeps what it quoted.
absolutizeUrls :: (String -> String) -> Maybe String -> String -> String
absolutizeUrls absolutize pageUrl = betweenTags
  where
    -- Rewriting happens inside a tag and nowhere else. A post quoting
    -- HTML -- in prose or, far more likely, in a fenced code block --
    -- contains the text `src="/res/a.png"` with no tag around it
    -- anywhere, and a post about HTML is exactly the post this would
    -- otherwise corrupt. (Inside a code block there is no literal `<`
    -- to begin with: pandoc has already escaped it to `&lt;`.)
    betweenTags [] = []
    betweenTags ('<' : c : cs)
      | isAlpha c || c == '/' = '<' : c : withinTag cs
    betweenTags (c : cs) = c : betweenTags cs

    withinTag [] = []
    withinTag ('>' : cs) = '>' : betweenTags cs
    -- Matched only after whitespace, so what matched is a whole
    -- attribute name: the `href` inside `data-href="..."` is not one.
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

-- | The attribute starting at the head of `s`, if it is one of
-- `urlAttrs` with a quoted value: its name, its quote character, the
-- value, and whatever follows the closing quote.
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

-- | Whether a URL carries a scheme of its own (@https:@, @mailto:@,
-- @data:@) and is therefore already absolute. The colon has to come
-- before any @\/@, @?@ or @#@ for it to be a scheme's -- otherwise
-- @img\/a:b.png@ would read as one.
hasScheme :: String -> Bool
hasScheme url = case break (== ':') url of
  (scheme@(c : _), ':' : _) -> isAlpha c && all schemeChar scheme
  _                         -> False

-- | Whether a character can appear in a URL scheme.
schemeChar :: Char -> Bool
schemeChar c = isAlphaNum c || c `elem` "+-."

-- | The directory a URL sits in, trailing @\/@ included, for resolving
-- a document-relative URL against it. Empty for a URL with no @\/@ at
-- all, which leaves such a URL untouched.
parentUrl :: String -> String
parentUrl = reverse . dropWhile (/= '/') . reverse

-- | Collapse a URL's @.@ and @..@ segments, so that joining a post's
-- page URL onto a relative one out of its own body produces the URL a
-- person would have written (@\/res\/img.svg@) rather than the one the
-- concatenation happens to spell (@\/post\/..\/res\/img.svg@). Both
-- fetch the same bytes from a browser, which normalizes before asking;
-- not everything that reads a feed does.
--
-- A @..@ with nothing left to climb is dropped, as the site root has no
-- parent -- and the authority is never what gets climbed into, so a URL
-- can't be normalized into a different host.
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

-- | A URL's @scheme:\/\/authority@, split from the path that follows it.
-- Empty origin for a URL that is only a path.
splitOrigin :: String -> (String, String)
splitOrigin url = case break (== ':') url of
  (scheme@(c : _), ':' : '/' : '/' : rest)
    | isAlpha c, all schemeChar scheme ->
        let (authority, path) = break (== '/') rest
         in (scheme ++ "://" ++ authority, path)
  _ -> ("", url)

-- | Split a URL path on @\/@, keeping the empty segments that record a
-- leading or trailing one.
splitSlashes :: String -> [String]
splitSlashes path = case break (== '/') path of
  (segment, '/' : rest) -> segment : splitSlashes rest
  (segment, _)          -> [segment]
