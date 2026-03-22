module Modules.Post where

import Modules.Config
import Modules.Utils.String

import System.FilePath
import System.IO (readFile)

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
  , postOutputPath :: FilePath
  , postUrl        :: Url
  , postContent    :: Markdown
  , postMeta       :: PostMeta
  } deriving (Show, Eq)

-- Loads a markdown file from `srcPath`, then resolves its derived paths/URL
-- and parses metadata from the front matter block.
parsePost :: FilePath -> IO Post
parsePost filename = do
  let name = takeBaseName filename
  let sourcePath = srcPath </> filename
  let outputPath = postPath </> filename
  let url = webPost ++ "/" ++ filename
  content <- readFile sourcePath 
  let meta = parsePostMeta $ splitFrontMatter sourcePath content 

  return Post 
    { postName = name
    , postSourcePath = sourcePath
    , postOutputPath = outputPath
    , postUrl  = url
    , postContent = content 
    , postMeta = meta}

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

-- Splits markdown front matter into key/value pairs.
--
-- The content must start with `metaDelimiter`, and the metadata section must
-- end with another `metaDelimiter`.
splitFrontMatter :: FilePath -> Markdown -> [(String, String)]
splitFrontMatter path content =
  case lines content of
    (metaDelimiter:rest) ->
      let (metaLines, remain) = break (== metaDelimiter) rest
      in case remain of
           [] -> error ("missing closing delimiter in " ++ path)
           otherwise -> map parseMetaLine metaLines
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
