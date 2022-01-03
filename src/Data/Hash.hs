module Data.Hash (
  Hash,
  bits,
  splitEvery
) where


import Data.Bits (Bits(..))
import Data.List (unfoldr)
import Data.Word (Word8, Word64)
import Numeric (showHex)

import Data.JSON (JSON(..))


newtype Hash = Hash (Word64, Word64) deriving (Eq, Ord)

instance Bits Hash where
  (Hash (a, b)) .&. (Hash (c, d)) = Hash (a .&. c, b .&. d)
  (Hash (a, b)) .|. (Hash (c, d)) = Hash (a .|. c, b .|. d)
  xor (Hash (a, b)) (Hash (c, d)) = Hash (xor a c, xor b d)
  complement (Hash (a, b)) = Hash (complement a, complement b)
  shiftL (Hash (a, b)) i =
    let mid = shiftR b (64 - i)  in
    Hash (shiftL a i .|. mid, shiftL b i)
  shiftR (Hash (a, b)) i =
    let mv = shiftL a (64 - i) in
    Hash (shiftL a i, shiftL b i .|. mv)
  rotateL (Hash (a, b)) i =
    let leftmost = shiftR a (64 - i)
        mid = shiftR b (64 - i) in
    Hash (shiftL a i .|. mid, shiftL b i .|. leftmost)
  rotateR (Hash (a, b)) i =
    let rightmost = shiftL b (64 - i)
        mid = shiftL a (64 - i) in
    Hash (shiftR a i .|. rightmost, shiftR b i .|. mid)
  bitSize (Hash _) = 128
  bitSizeMaybe (Hash _) = Just 128
  isSigned (Hash _) = False
  bit i
    | i < 64 = Hash (0, bit i)
    | otherwise = Hash (bit (i - 64), 0)
  testBit (Hash (a, b)) i
    | i < 64 = testBit b i
    | otherwise = testBit a i
  popCount (Hash (a, b)) = popCount a + popCount b

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n (x : xs) = snd $ foldl doSplit (n - 1, [[x]]) xs
  where
  doSplit :: (Int, [[a]]) -> a -> (Int, [[a]])
  doSplit (count, (l : ls)) elem
    | count > 0 = (count - 1, (elem : l) : ls)
    | otherwise = (n - 1, [elem] : l : ls)

bits :: Bits b => [Int] -> b
bits = foldl (\w b -> w .|. bit b) zeroBits

splitChunks :: Word64 -> [Word8]
splitChunks = unfoldr takeBits . (,) 56
  where
  takeBits :: (Int, Word64) -> Maybe (Word8, (Int, Word64))
  takeBits (b, w)
    | b >= 0 =
      let mask = bits [b..(b+7)] in
      Just (fromIntegral $ shiftR w b, (b - 8, w .&. complement mask))
    | otherwise = Nothing

instance Show Hash where
  show (Hash (a, b)) = foldl1 (.) bytes ""
    where
    bytes = map withPadding (splitChunks a <> splitChunks b)
    withPadding :: Word8 -> ShowS
    withPadding w
      | w > 15 = showHex w
      | otherwise = showHex 0 . showHex w
