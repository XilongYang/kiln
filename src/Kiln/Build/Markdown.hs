-- | The pure source transforms `Kiln.Build.Post` applies to a post
-- before handing it to pandoc, and the one it applies to pandoc's
-- rendered table of contents afterwards.
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
  ) where

import Data.Char (isAlphaNum, isSpace)
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
