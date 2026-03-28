module Modules.Post.Parse (parsePost) where

import Data.List (isPrefixOf)
import Modules.Post
import Modules.TypeAlias
import Modules.Utils.String
import System.FilePath

-- ---[ Overview ]------------------------------------------------------------
-- | Markdown post parser.
--
-- This module reads a source post file and converts it into the canonical
-- 'Post' structure used by downstream build stages.

-- ---[ Public API ]------------------------------------------------------------

-- | Parses one markdown post file into a structured 'Post' value.
--
-- Expected source shape:
-- - front matter delimited by @metaDelimiter@
-- - post body markdown content after front matter
--
-- This parser is strict about malformed metadata and will fail on invalid
-- front-matter layout, malformed metadata lines, or missing required keys.
parsePost :: FilePath -> IO Post
parsePost pathToParse = do
  let name = takeBaseName pathToParse
  let sourcePath = pathToParse
  rawContent <- readFile sourcePath 
  let (rawMeta, rest) = splitFrontMatter sourcePath rawContent
  let (abstract, body) = extractPostAbstract rest

  return Post 
    { postName = name
    , postSourcePath = sourcePath 
    , postBody = body
    , postAbstract = abstract 
    , postMeta = extractMetaFrom rawMeta}

-- ---[ Implementation Details ]-----------------------------------------------

-- | Splits source markdown into parsed metadata pairs and remaining body.
--
-- Fails when front matter structure is invalid or any metadata line fails to
-- parse (see 'parseMetaLine').
splitFrontMatter :: FilePath -> Markdown -> ([(String, String)], Markdown)
splitFrontMatter path content =
  case lines content of
    (metaDelimiter:rest) ->
      let (metaLines, remain) = break (== metaDelimiter) rest
      in case remain of
           (metaDelimiter:body) -> (map parseMetaLine metaLines, unlines body)
           otherwise -> error ("missing closing delimiter in " ++ path)
    _ -> error ("missing opening delimiter in " ++ path)

-- | Parses one metadata line in @key: value@ form.
--
-- Fails when:
-- - line is empty
-- - separator @:@ is missing
-- - key is empty
-- - value is empty
--
-- Notes:
-- - surrounding spaces are trimmed from both key and value
-- - only the first @:@ is treated as separator; later @:@ stay in value
parseMetaLine :: String -> (String, String)
parseMetaLine [] = error ("Empty line(s) in meta block!")
parseMetaLine str = checkPair . trimPair . (break (== ':')) $ str
  where
    errorMsg :: String
    errorMsg = "Invalid metadata line: " ++ str

    checkPair :: (String, String) -> (String, String)
    checkPair ([], _) = error errorMsg
    checkPair (_, []) = error errorMsg
    checkPair pass = pass

    trimPair :: (String, String) -> (String, String)
    trimPair (a, b:bs) = (trim a, trim bs)
    trimPair _ = error errorMsg

-- | Splits markdown into abstract and body at the first level-2 heading.
--
-- The abstract is everything before the first line starting with @\"## \"@.
extractPostAbstract :: Markdown -> (Markdown, Markdown)
extractPostAbstract content = merge $ break (isPrefixOf "## ") $ lines content
  where merge (a, b) = (unlines a, unlines b)

-- | Builds 'PostMeta' from parsed key/value metadata pairs.
--
-- Requires @title@, @author@, and @date@ keys.
-- Missing keys cause an error.
extractMetaFrom :: [(String, String)] -> PostMeta
extractMetaFrom pairs =
  PostMeta
    { metaTitle  = getOrError "title"
    , metaAuthor = getOrError "author"
    , metaDate   = getOrError "date"
    }
  where
    getOrError :: String -> String
    getOrError key =
      case lookup key pairs of
        Just value -> value
        Nothing    -> error ("Missing value of meta key: " ++ key)
