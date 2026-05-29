module Modules.Post.Parse (parsePost, ParsePostError (..)) where

import Data.List (isPrefixOf)
import Modules.Post
import Modules.Post.Internal
import Modules.TypeAlias
import Modules.Utils.String
import Control.Monad ((>=>))

-- ---[ Overview ]------------------------------------------------------------
-- | Markdown post parser.
--
-- This module parses one source markdown file into a canonical 'Post'.
-- The parser expects YAML-like front matter delimited by @metaDelimiter@,
-- then regular markdown body content.
--
-- Parsing is split into:
-- - 'parsePost' for file IO + pure parsing
-- - 'parsePostPure' for content validation and data construction

-- ---[ Public API ]------------------------------------------------------------

-- | Parse errors for a single post file.
data ParsePostError
  = MissingOpeningDelimiter -- ^ First line is not @metaDelimiter@.
  | MissingClosingDelimiter -- ^ Metadata block has no closing delimiter.
  | EmptyMetaLine -- ^ Empty line appears inside metadata block.
  | InvalidMetaLine String -- ^ Metadata line is not valid @key: value@.
  | MissingMetaField String -- ^ Required metadata key is missing.
  deriving (Show, Eq)


-- | Parses one markdown file into 'Post'.
--
-- Input is read from disk, then validated by 'parsePostPure'.
--
-- Error model:
-- - file read failures are represented by the outer 'IO'
-- - content/format failures are represented by 'Left ParsePostError'
parsePost :: FilePath -> IO (Either ParsePostError Post)
parsePost pathToParse = do
  rawContent <- readFile pathToParse
  pure $ parsePostPure rawContent 

-- ---[ Implementation Details ]-----------------------------------------------

-- | Parsed metadata pair: @(key, value)@.
type MetaPair = (String, String)

-- | Pure parser used by 'parsePost' after reading file content.
--
-- Steps:
-- - split front matter and body
-- - parse and validate metadata lines
-- - ensure required fields exist
-- - derive abstract/body sections
parsePostPure :: Markdown -> Either ParsePostError Post
parsePostPure rawContent = do
  (rawMeta, rest) <- splitFrontMatter rawContent
  meta <- extractMetaFrom rawMeta
  let (abstract, body) = extractPostAbstract rest

  pure Post 
    { postBody = body
    , postAbstract = abstract 
    , postMeta = meta}


-- | Splits markdown into parsed metadata and body text.
--
-- Expected shape:
-- - first line is @metaDelimiter@
-- - metadata lines follow
-- - next @metaDelimiter@ closes metadata
-- - remaining lines are body
--
-- Fails with delimiter errors or metadata-line errors.
splitFrontMatter :: Markdown -> Either ParsePostError ([MetaPair], Markdown)
splitFrontMatter content = do
  rest <- checkDelimiter (lines content) MissingOpeningDelimiter
  let (metaLines, remain) = break (== metaDelimiter) rest
  body <- checkDelimiter remain MissingClosingDelimiter
  metaPairs <- traverse parseMetaLine metaLines
  pure $ (metaPairs, unlines body)

-- | Validates a delimiter line and returns the remaining lines.
--
-- Returns the provided error when:
-- - input is empty
-- - first line is not @metaDelimiter@
checkDelimiter :: [String] -> ParsePostError -> Either ParsePostError [String]
checkDelimiter [] err = Left err
checkDelimiter (firstLine: rest) err
  | firstLine == metaDelimiter = Right rest
  | otherwise = Left err

-- | Parses one metadata line in @key: value@ form.
--
-- Validation rules:
-- - line is empty
-- - separator @:@ is missing
-- - key is empty
-- - value is empty
--
-- Notes:
-- - surrounding spaces are trimmed from both key and value
-- - only the first @:@ is used as separator; later @:@ remain in value
parseMetaLine :: String -> Either ParsePostError MetaPair
parseMetaLine [] = Left EmptyMetaLine
parseMetaLine line = (trimPair >=> checkPair) . (break (== ':')) $ line
  where
    checkPair :: (String, String) -> Either ParsePostError (String, String)
    checkPair ([], _) = Left $ InvalidMetaLine line
    checkPair (_, []) = Left $ InvalidMetaLine line
    checkPair pass = Right pass

    trimPair :: (String, String) -> Either ParsePostError (String, String)
    trimPair (a, ':':bs) = Right (trim a, trim bs)
    trimPair _ = Left $ InvalidMetaLine line

-- | Splits markdown into abstract and body at first level-2 heading.
--
-- Abstract is all lines before the first line prefixed by @\"## \"@.
-- Body starts from that heading line (inclusive). If no such heading exists,
-- body is empty and abstract contains the full content.
extractPostAbstract :: Markdown -> (Markdown, Markdown)
extractPostAbstract content = merge $ break (isPrefixOf "## ") $ lines content
  where merge (a, b) = (unlines a, unlines b)

-- | Builds 'PostMeta' from parsed metadata pairs.
--
-- Required keys:
-- - @title@
-- - @author@
-- - @date@
--
-- Missing keys return 'MissingMetaField'.
extractMetaFrom :: [MetaPair] -> Either ParsePostError PostMeta
extractMetaFrom pairs = do
  title <- getOrError "title"
  author <- getOrError "author"
  date <- getOrError "date"
  pure $ PostMeta title author date
  where
    getOrError :: String ->Either ParsePostError String
    getOrError key =
      case lookup key pairs of
        Just value -> Right value
        Nothing    -> Left $ MissingMetaField key
