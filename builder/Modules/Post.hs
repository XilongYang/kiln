module Modules.Post where

import Modules.Utils.String

import System.FilePath
import System.IO (readFile)
import Data.List (isPrefixOf)

-- Canonical post filename stem (without extension).
type PostName = String
-- Raw markdown text of the post file.
type Markdown = String
-- Public URL for the generated post page.
type Url = String

-- Structured metadata extracted from markdown front matter.
data PostMeta = PostMeta
  { metaTitle  :: String
  , metaAuthor :: String
  , metaDate   :: String
  } deriving (Show, Eq)

-- Full in-memory representation of a post source file.
data Post = Post
  { postName       :: PostName
  , postSourcePath :: FilePath
  -- Renderable markdown content after abstract extraction.
  , postContent    :: Markdown
  -- Lead markdown snippet extracted before the main section marker.
  , postAbstract   :: Markdown
  , postMeta       :: PostMeta
  } deriving (Show, Eq)

-- Loads a markdown file, parses front matter metadata, and splits the body
-- into abstract/main sections.
parsePost :: FilePath -> IO Post
parsePost pathToParse = do
  let name = takeBaseName pathToParse
  let sourcePath = pathToParse
  rawContent <- readFile sourcePath 
  let (rawMeta, body) = splitFrontMatter sourcePath rawContent
  let (abstract, content) = extractPostAbstract body 

  return Post 
    { postName = name
    , postSourcePath = sourcePath 
    , postContent = content 
    , postAbstract = abstract 
    , postMeta = parsePostMeta rawMeta}

-- Splits post body into `(abstract, content)` around the first heading marker.
--
-- The input is expected to be markdown lines after front matter handling.
extractPostAbstract :: Markdown -> (Markdown, Markdown)
extractPostAbstract content = merge $ break (isPrefixOf "## ") $ lines content
  where merge (a, b) = (unlines a, unlines b)

-- Projects known metadata keys into `PostMeta`.
--
-- Missing keys are treated as empty strings.
parsePostMeta :: [(String, String)] -> PostMeta
parsePostMeta pairs =
  PostMeta
    { metaTitle  = getOrEmpty "title"
    , metaAuthor = getOrEmpty "author"
    , metaDate   = getOrEmpty "date"
    }
  where
    getOrEmpty key =
      case lookup key pairs of
        Just value -> value
        Nothing    -> ""

-- Splits markdown front matter into `(metadata, remain-content)`.
--
-- The content must start with `metaDelimiter`, and the metadata section must
-- end with another `metaDelimiter`.
splitFrontMatter :: FilePath -> Markdown -> ([(String, String)], Markdown)
splitFrontMatter path content =
  case lines content of
    (metaDelimiter:rest) ->
      let (metaLines, remain) = break (== metaDelimiter) rest
      in case remain of
           (metaDelimiter:body) -> (map parseMetaLine metaLines, unlines body)
           otherwise -> error ("missing closing delimiter in " ++ path)
    _ -> error ("missing opening delimiter in " ++ path)

-- Front matter delimiter used in markdown posts.
metaDelimiter :: String
metaDelimiter  = "---"

-- Parses a single metadata line in `key: value` form.
--
-- Empty input returns `("", "")`. Non-empty input without `:` throws.
parseMetaLine :: String -> (String, String)
parseMetaLine [] = ([], [])
parseMetaLine str = trimPair $ break (== ':') str
  where
    trimPair (a, b:bs) = (trim a, trim bs)
    trimPair _ = error ("invalid metadata line: " ++ str)

-- Serializes `PostMeta` back to canonical markdown front matter.
--
-- The output always contains both opening and closing `metaDelimiter` lines
-- and writes keys in the fixed order: title, author, date.
revertMeta :: PostMeta -> String
revertMeta meta = unlines 
  [ metaDelimiter 
  , "title: " ++ metaTitle meta
  , "author: " ++ metaAuthor meta
  , "date: " ++ metaDate meta
  , metaDelimiter
  ]
