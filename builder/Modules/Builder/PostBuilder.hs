module Modules.Builder.PostBuilder where

import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.String
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.Process (callProcess)

-- Builds the preprocessed markdown payload used before HTML rendering.
--
-- Layout:
-- 1) serialized front matter
-- 2) wrapped abstract block
-- 3) table-of-contents marker
-- 4) body markdown with rewritten fenced-code marks
genPreprocessedPostText :: Post -> String
genPreprocessedPostText post = unlines
  [ rawMeta
  , abstract
  , "[[toc]]\n"
  , rewriteLanguageMarks $ postBody post
  ]
  where
    rawMeta = revertMeta $ postMeta post
    abstract = unlines
      [ "<div class=\"abstract\">"
      , postAbstract post
      , "</div>"
      ]

-- Rewrites all lines so fenced-code language marks are normalized.
rewriteLanguageMarks :: String -> String
rewriteLanguageMarks = 
  unlines . map rewriteLanguageMarkLine . lines

-- Rewrites one markdown line if it is a fenced-code opening with language tag.
--
-- Example:
-- - input:  ```haskell
-- - output: ``` {.language-haskell .line-numbers .match-braces}
rewriteLanguageMarkLine :: String -> String
rewriteLanguageMarkLine line
  | not $  "```" `isPrefixOf` line = line
  | mark == "" = line
  | otherwise = replace "[mark]" mark "``` {.language-[mark] .line-numbers .match-braces}"
  where
    mark = trim $ drop 3 line

-- Builds pandoc CLI arguments for markdown-to-html conversion.
--
-- Parameter order:
-- 1) input markdown path
-- 2) template html path
-- 3) output html path
mkPandocArgs :: FilePath -> FilePath -> FilePath -> [String]
mkPandocArgs inputPath templatePath outputPath =
  [ inputPath
  , "-o", outputPath
  , "--template=" ++ templatePath
  , "--no-highlight"
  , "--mathjax"
  , "--wrap=none"
  ]

-- Executes pandoc with standardized flags used by the builder.
runPandoc :: FilePath -> FilePath -> FilePath -> IO ()
runPandoc inputPath templatePath outputPath = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  callProcess "pandoc" (mkPandocArgs inputPath templatePath outputPath)

-- Injects generated TOC HTML in place of the `[[toc]]` marker.
genToc :: Html -> Html
genToc html = replace "[[toc]]" toc html
  where 
    toc = unlines
      [ "<nav role=\"navigation\" class=\"toc\">"
      , "<h2>Contents"
      , "<i class=\"material-icons icon\" id=\"toc-control\">remove</i>"
      , "</h2>"
      , "<div id=\"toc-items\">"
      , tocs html
      , "</nav>"
      ]
    (_, main) = ((break (== "<main>")) . (map trim) . lines) html
    headItems = catMaybes $ map extractHeadItem main
    tocs html = "<ol>" ++ tocItems headItems ++ "</ol>"

-- Parses one heading line like `<h2 id="...">Title</h2>`.
extractHeadItem :: Html -> Maybe (String, String, String)
extractHeadItem line
  | not ("<h" `isPrefixOf` line) = Nothing
  | otherwise = case break (== '>') line of
    (openTag, '>':rest) ->
      let level = [openTag !! 2]
          ident = extractIdent openTag 
          title = takeWhile (/= '<') rest
      in Just (level, ident, title)
    otherwise -> Nothing
  where
    extractIdent [] = ""
    extractIdent (x:xs) 
      | "id=\"" `isPrefixOf` xs = takeWhile (/= '"') (drop 4 xs)
      | otherwise = extractIdent xs

-- Folds a flat heading list into nested `<ol>` blocks by heading level.
tocItems :: [(String, String, String)] -> Html
tocItems [] = ""
tocItems ((level, ident, title) : xs) = unlines
  [ "<li><a href=\"#" ++ ident ++"\">" ++ title ++ "</a><ol>"
  , tocItems subItems
  , "</ol></li>"
  , tocItems restItems
  ]
  where
    (subItems, restItems) = break (\(curLevel, _, _) -> level == curLevel) xs
