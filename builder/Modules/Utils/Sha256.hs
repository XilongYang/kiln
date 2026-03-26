module Modules.Utils.Sha256 (sha256Hex) where

import Data.Word
import Data.Bits
import Data.List
import Numeric (showHex)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as ByteString

-- ==============================================================================
-- Interface
-- ==============================================================================

sha256Hex :: String -> String
sha256Hex = (concatMap byteToHex) . sha256 . utf8Encode
  where
    byteToHex :: Word8 -> String
    byteToHex b =
      let s = showHex b ""
      in if length s == 1 then '0' : s else s
    
    utf8Encode :: String -> [Word8]
    utf8Encode = ByteString.unpack . TextEncoding.encodeUtf8 . Text.pack

-- ==============================================================================
-- Constant
-- ==============================================================================

-- SHA-256 operates on 512-bit (64-byte) chunks.
chunkSize :: Int
chunkSize = 64

h :: [Word32]
h = [ 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a
    , 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

k :: [Word32]
k = [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b
    , 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01
    , 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7
    , 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
    , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152
    , 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147
    , 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc
    , 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
    , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819
    , 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08
    , 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f
    , 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
    , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

messageSizeFieldBytes :: Int
messageSizeFieldBytes = 8

initialWordsCount :: Int
initialWordsCount = 16

wordsCount :: Int
wordsCount = 64

s0 :: Word32 -> Word32
s0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3

s1 :: Word32 -> Word32
s1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

ep0 :: Word32 -> Word32
ep0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22

ep1 :: Word32 -> Word32
ep1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` (complement x .&. z) 

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

-- ==============================================================================
-- Process
-- ==============================================================================

sha256 :: [Word8] -> [Word8]
sha256 msg = concatMap w32ToW8s finalH
  where
    initialWords :: [[Word32]]
    initialWords = (splitChunksToWords . splitToChunks . padding) msg

    expandedWords :: [[Word32]]
    expandedWords = map expandWords initialWords

    processChunk :: [Word32] -> [Word32] -> [Word32]
    processChunk hs ws
      | length hs /= 8 = error "Wrong initial H for processChunk."
      | length ws /= wordsCount = error "Wrong words for processChunk."
      | otherwise =
          -- Start from previous hash state and run 64 compression rounds.
          let finalWorking = foldl' (refreshH ws) hs [0 .. wordsCount - 1]
          in zipWith (+) hs finalWorking
    
    finalH :: [Word32]
    finalH = foldl' processChunk h expandedWords

    w32ToW8s :: Word32 -> [Word8]
    w32ToW8s x =  
      [ fromIntegral (shiftR x 24)
      , fromIntegral (shiftR x 16)
      , fromIntegral (shiftR x 8)
      , fromIntegral x
      ]

-- Pads a message to the SHA-256 block format:
-- original bytes + 0x80 + zeroes + 64-bit big-endian bit length.
padding :: [Word8] -> [Word8]
padding msg = initBytes ++ replicate paddingSize continueByte ++ msgSizeBytes
  where
    firstByte :: Word8
    firstByte = 0x80

    continueByte :: Word8
    continueByte = 0x00
    
    initBytes :: [Word8]
    initBytes = msg ++ [firstByte]

    modSize :: Int
    modSize = length initBytes `mod` chunkSize

    targetModSize :: Int
    targetModSize = chunkSize - messageSizeFieldBytes

    paddingSize :: Int
    paddingSize = (targetModSize - modSize) `mod` chunkSize

    msgSizeBytes :: [Word8]
    msgSizeBytes = intToBE64 (fromIntegral (length msg) * 8)

    intToBE64 :: Integer -> [Word8]
    intToBE64 x =  
      let w = fromIntegral x :: Word64
      in
      [ fromIntegral (shiftR w 56)
      , fromIntegral (shiftR w 48)
      , fromIntegral (shiftR w 40)
      , fromIntegral (shiftR w 32)
      , fromIntegral (shiftR w 24)
      , fromIntegral (shiftR w 16)
      , fromIntegral (shiftR w 8)
      , fromIntegral w
      ]

splitToChunks :: [Word8] -> [[Word8]]
splitToChunks msg 
  | length msg `mod` chunkSize /= 0 = error "Message length incorrect!"
  | otherwise = splitToChunks' msg
  where
    splitToChunks' :: [Word8] -> [[Word8]]
    splitToChunks' [] = []
    splitToChunks' msg' = 
      let (chunk, rest) = splitAt chunkSize msg'
      in chunk : splitToChunks' rest

splitChunksToWords :: [[Word8]] -> [[Word32]]
splitChunksToWords = map splitChunkToWords
  where
    splitChunkToWords :: [Word8] -> [Word32]
    splitChunkToWords chunk
      | length chunk `mod` 4 /= 0 = error "Chunk size incorrect!"
      | otherwise = splitChunkToWords' chunk

    splitChunkToWords' :: [Word8] -> [Word32]
    splitChunkToWords' [] = []
    splitChunkToWords' word8s =
      let (wordGroup, rest) = splitAt 4 word8s
      in bytesToWord32 wordGroup : splitChunkToWords' rest
    
    bytesToWord32 :: [Word8] -> Word32
    bytesToWord32 [w0,w1,w2,w3] = 
      shiftL (fromIntegral w0) 24 .|. 
      shiftL (fromIntegral w1) 16 .|. 
      shiftL (fromIntegral w2)  8 .|. 
      fromIntegral w3 

expandWords :: [Word32] -> [Word32]
expandWords initialWords
  | length initialWords /= initialWordsCount = error "Counts of initial words must be 16"
  | otherwise = expandWords' initialWords initialWordsCount
  where
    expandWords' :: [Word32] -> Int -> [Word32]
    expandWords' ws i
      | i == 64 = ws
      | otherwise = expandWords' (ws ++ [nextWord ws i]) (i + 1)

    nextWord :: [Word32] -> Int -> Word32
    nextWord ws i = (ws !! (i - 16)) + s0(ws !! (i - 15)) + ws !! (i - 7) + s1(ws !! (i - 2))


refreshH :: [Word32] -> [Word32] -> Int -> [Word32]
refreshH ws h' i
  | i < 0 || i >= wordsCount = error "Invalid number for refresh H."
  | otherwise = 
    case h' of 
      [h0, h1, h2, h3, h4, h5, h6, h7] ->
        let t1 = h7 + ep1 h4 + (ch h4 h5 h6) + (k !! i) + (ws !! i)
            t2 = ep0 h0 + maj h0 h1 h2
        in [t1 + t2, h0, h1, h2, h3 + t1, h4, h5, h6]
      _ -> error "Wrong initial H for refresh H."

