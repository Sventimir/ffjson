module Data.Hash (
  Hash,
  RollingHash(..),
  hash,
  saltedHash,
  hashEnum
) where

import Data.Bits (Bits(..))
import Data.List (unfoldr, sortBy)
import Data.Ratio (numerator, denominator)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word64)
import Numeric (showHex)

import Data.JSON (JSON(..))


newtype Hash = Hash (Word64, Word64) deriving (Eq, Ord)

foldNatDown :: (a -> Int -> a) -> (Int -> Bool) -> a -> Int -> a
foldNatDown _ _ acc 0 = acc
foldNatDown f p acc count
  | p count = foldNatDown f p (f acc count) (count - 1)
  | otherwise = foldNatDown f p acc (count - 1)

instance Bits Hash where
  (Hash (a, b)) .&. (Hash (c, d)) = Hash (a .&. c, b .&. d)
  (Hash (a, b)) .|. (Hash (c, d)) = Hash (a .|. c, b .|. d)
  xor (Hash (a, b)) (Hash (c, d)) = Hash (xor a c, xor b d)
  complement (Hash (a, b)) = Hash (complement a, complement b)
  shiftL (Hash (a, b)) i =
    let mid = shift b (64 - i) in
    Hash (shiftL a i .|. mid, shiftL b i)
  shiftR (Hash (a, b)) i =
    let mid = shift a (64 - i) in
    Hash (shiftL a i, shiftL b i .|. mid)
  rotateL (Hash (a, b)) i =
    let leftmost = shift a (64 - i)
        mid = shift b (64 - i) in
    Hash (shiftL a i .|. mid, shiftL b i .|. leftmost)
  rotateR (Hash (a, b)) i =
    let rightmost = shift b (64 - i)
        mid = shift a (64 - i) in
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

instance Bounded Hash where
  minBound = Hash (minBound, minBound)
  maxBound = Hash (maxBound, maxBound)

instance Enum Hash where
  toEnum = fromIntegral
  fromEnum (Hash (_, i)) = fromIntegral i

instance Num Hash where
  (Hash (a, b)) + (Hash (c, d)) =
    let l = a + c
        r = b + d in
    if r < b then {- carry -} Hash (l + 1, r) else Hash (l, r)
  a * b = foldNatDown (\w pos -> w + shiftL a pos) (testBit b) zeroBits 63
  abs = id
  signum (Hash _) = Hash (0, 1)
  fromInteger i = Hash (fromInteger $ shiftR i 64, fromInteger i)
  negate (Hash (0, 0)) = Hash (0, 0)
  negate (Hash (a, b)) = Hash (maxBound - a, maxBound - b + 1)

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
      | otherwise = showHex (0 :: Int) . showHex w

newtype RollingHash = RollingHash (Hash -> Hash)

defaultSeed :: Hash
defaultSeed = 0x270012ea13770132d98134e14aff0123

hashEnum :: Enum c => c -> Hash
hashEnum e = snd $ foldNatDown apply (const True) (initial, defaultSeed) 32
  where
  initial :: Hash
  initial = fromIntegral $ fromEnum e
  apply :: (Hash, Hash) -> Int -> (Hash, Hash)
  apply (h, acc) pos = (rotateL (xor acc h) 2, acc + rotateL h (4 * pos))
  

hashString :: Text -> Hash -> Hash
hashString = flip (Text.foldl' absorbChar)
  where
  absorbChar :: Hash -> Char -> Hash
  absorbChar h c = h `xor` hashEnum c 

hashNum :: Rational -> Hash -> Hash
hashNum n h =
  h `xor` hashEnum (hashEnum (numerator n) `xor` hashEnum (denominator n))

hashBool :: Bool -> Hash
hashBool True = 0x632d8abc541004ccfe021456ecd21933
hashBool False = 0x2def321302ffac31b6b2189c0c9b12a0

-- Because hashes are used for comparison, it is important that objects
-- differing just by the order of their properties have the same hashes.
sortObj :: [(Text, a)] -> [(Text, a)]
sortObj = sortBy (\(k, _) (k', _) -> compare k k')

hashObj :: (Text, RollingHash) -> RollingHash
hashObj (k, RollingHash vh) =
  RollingHash $ \h -> rotateL (hashString k $ rotateR (vh h) 1) 1

instance JSON RollingHash where
  str txt = RollingHash (flip rotateL 3 . hashString txt)
  num dbl = RollingHash (flip rotateL 6 . hashNum dbl)
  bool b = RollingHash (flip rotateL 9 . xor (hashBool b))
  null = RollingHash $ flip rotateL 12
  array = concatRollingHashes (RollingHash $ flip rotateR 15)
  obj = concatRollingHashes (RollingHash $ flip rotateL 18) . map hashObj . sortObj

instance Show RollingHash where
  show = show . hash
  
concatRollingHashes :: RollingHash -> [RollingHash] -> RollingHash
concatRollingHashes = foldl concatHash
  where
  concatHash (RollingHash f) (RollingHash g) = RollingHash (g . f)

saltedHash :: Hash -> RollingHash -> Hash
saltedHash salt (RollingHash h) = h salt

hash :: RollingHash -> Hash
hash = saltedHash 1234567890123456789012345678901234567890
