-- | Utilities for rendering and parsing keyed-line-block (KLB) text.
--
-- This module defines:
-- - KLB field/block type aliases
-- - the 'Klb' type class for block encoding/decoding
-- - helpers that serialize/parse textual KLB blocks
--
-- Format used by this module:
-- - one block starts with @size:N@
-- - followed by exactly @N@ lines of @key:value@
-- - repeated for each subsequent block
module Modules.Utils.Klb 
  ( KlbRenderError
  , KlbParseError
  , Klb(..)
  , KlbBlock
  , KlbPair
  , renderKlb
  , parseKlb) where

import Modules.Utils.String
import Text.Read (readMaybe)

-- ---[ Public API ]------------------------------------------------------------

-- | Render failures while encoding values into textual KLB data.
--
-- This type is part of the public API of this module.
data KlbRenderError
  = EmptyBlock
  -- ^ A block has zero fields and cannot be rendered.
  | MissingKey KlbPair
  -- ^ A field key is empty.
  | InvalidKey String
  -- ^ A field key contains @:@.
  deriving (Show, Eq)

-- | Parse failures while decoding textual KLB data.
--
-- Each constructor pinpoints one structural parse failure.
data KlbParseError
  = InvalidLine String
  -- ^ A line does not match the expected @key:value@ shape.
  | InvalidHeaderKey String
  -- ^ Header key must be @size@.
  | InvalidSize Int
  -- ^ Parsed size is non-positive.
  | InvalidSizeValue String
  -- ^ Header value for @size@ is not an integer.
  | IncompleteBlock Int Int
  -- ^ Expected N payload lines but got fewer (expected, actual).
  deriving (Show, Eq)

-- | Bidirectional conversion between a domain value and one KLB block.
--
-- 'fromKlbBlock' is intentionally total at the type level.
-- Structural validation belongs to text parsing ('parseKlbLines' and helpers).
class Klb a where
  -- | Encode one value into a single KLB block.
  toKlbBlock   :: a -> KlbBlock
  -- | Decode one KLB block back into a domain value.
  fromKlbBlock :: KlbBlock -> a

-- | One KLB block: an ordered list of fields.
type KlbBlock = [KlbPair]

-- | One KLB field represented as @(key, value)@.
type KlbPair = (String, String)

-- | Render values into textual KLB format.
--
-- Parameters:
-- - list of values that implement 'Klb'
--
-- Output:
-- - each value becomes one block
-- - each block starts with its own @size:N@ header line
-- - each block contains one @key:value@ line per field
-- - blocks are concatenated directly (no extra blank separator line)
--
-- Validation:
-- - any empty block fails with 'EmptyBlock'
-- - any invalid key fails with 'MissingKey' or 'InvalidKey'
renderKlb :: Klb a => [a] -> Either KlbRenderError String
renderKlb objs = do
  let blocks = map toKlbBlock objs
  renderedBlocks <- traverse renderBlock blocks
  pure $ concat renderedBlocks
 
-- | Parse KLB text into domain values.
--
-- Input is split by lines, grouped into blocks by 'parseKlbLines',
-- then each block is decoded via 'fromKlbBlock'.
--
-- Returns:
-- - 'Left' when block framing/header/line structure is invalid
-- - 'Right' when all blocks are structurally decoded successfully
--
-- This parser expects each block to begin with its own @size:N@ header.
-- Extra blank lines are treated as invalid lines.
parseKlb :: Klb a => String -> Either KlbParseError [a]
parseKlb str = do
  blocks <- parseKlbLines (lines str)
  pure $ map fromKlbBlock blocks

-- ---[ Implementation Details ]-----------------------------------------------

-- | Header key used to declare payload size.
headerKeySize :: String
headerKeySize = "size"

-- | Render one block as multiple lines.
--
-- Output layout:
-- - first line: @size:<N>@
-- - then @N@ rendered @key:value@ lines
--
-- Validation:
-- - empty blocks are rejected
--
-- The returned text always ends with a trailing newline.
renderBlock :: KlbBlock -> Either KlbRenderError String
renderBlock block 
  | length block <=0 = Left EmptyBlock
  | otherwise = do
    let header = headerKeySize ++ ":" ++ show (length block) 
    pairs <- traverse renderLine block
    let body = unlines pairs
    pure $ header ++ "\n" ++ body 

-- | Render a single @(key, value)@ pair as @key:value@.
--
-- Validation:
-- - key must be non-empty
-- - key must not contain @:@
-- - value is whitespace-normalized via 'flattenText'
renderLine :: KlbPair -> Either KlbRenderError String
renderLine pair@("", _) = Left $ MissingKey pair
renderLine (k, v) = case break (== ':') k of
  (_, []) -> Right $ flattenText k ++ ":" ++ flattenText v
  _       -> Left $ InvalidKey k

-- | Normalize whitespace by collapsing consecutive spaces/newlines/tabs.
flattenText :: String -> String
flattenText = trim . unwords . words

-- | Parse raw KLB lines into blocks.
--
-- Consumes one header + payload chunk at a time, then recurses on the rest.
-- Parsing is strict: blank lines are treated as invalid lines (not skipped).
--
-- For each step:
-- - parse current header via 'extractChunkSize'
-- - take exactly that many payload lines
-- - parse payload lines as key-value pairs
parseKlbLines :: [String] -> Either KlbParseError [KlbBlock]
parseKlbLines [] = Right []
parseKlbLines (header : body) = do
    chunkSize <- extractChunkSize header
    (chunk, rest) <- splitAtWithError chunkSize body
    block <- traverse parseKlbLine chunk
    restBlocks <- parseKlbLines rest
    pure $ block : restBlocks 

-- | Split payload lines into one exact-size chunk and remaining lines.
--
-- Fails when the declared size is non-positive or larger than remaining input.
-- This means @size:0@ is currently invalid.
splitAtWithError :: Int -> [String] -> Either KlbParseError ([String], [String])
splitAtWithError size str
  | size <= 0 = Left (InvalidSize size)
  | length str < size = Left $ IncompleteBlock size (length str)
  | otherwise = Right $ splitAt size str

-- | Read the header line and extract declared chunk size.
--
-- Header must be in @size:N@ form.
-- A non-@size@ key becomes 'InvalidHeaderKey'.
extractChunkSize :: String -> Either KlbParseError Int
extractChunkSize header = do
  (k, v) <- parseKlbLine header
  if k /= headerKeySize 
    then Left (InvalidHeaderKey k)
    else parseSizeValue v

-- | Parse an integer size from header value text.
parseSizeValue :: String -> Either KlbParseError Int
parseSizeValue v =
  case readMaybe v of
    Nothing -> Left (InvalidSizeValue v)
    Just n  -> Right n

-- | Parse one textual @key:value@ line.
--
-- Splits only at the first @:@, so values may contain @:@.
-- Empty keys are rejected.
-- Whitespace is kept as-is at parse time.
parseKlbLine :: String -> Either KlbParseError KlbPair
parseKlbLine line = 
  case break (== ':') line of
    ("", _)      -> Left (InvalidLine line)
    (k, ':' : v) -> Right (k, v)
    _            -> Left (InvalidLine line)
