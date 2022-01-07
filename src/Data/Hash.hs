module Data.Hash (
  Hash,
  RollingHash(..),
  hash,
  saltedHash
) where


import Data.Bits (Bits(..))
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word64)
import Numeric (showHex)

import Data.JSON (JSON(..))


newtype Hash = Hash (Word64, Word64) deriving (Eq, Ord)

foldNatDown :: (a -> Int -> a) -> (Int -> Bool) -> a -> Int -> a
foldNatDown _ _ acc 0 = acc
foldNatDown f pred acc count
  | pred count = foldNatDown f pred (f acc count) (count - 1)
  | otherwise = foldNatDown f pred acc (count - 1)

foldNatUp :: (a -> Int -> a) -> (Int -> Bool) -> a -> Int -> a
foldNatUp f pred init limit = doFold init 0
  where
  doFold acc count
    | count > limit = acc
    | pred count = doFold (f acc count) (count + 1)
    | otherwise = doFold acc (count + 1)

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

newtype RollingHash = RollingHash (Hash -> Hash)

defaultSeed :: Hash
defaultSeed = 0x270012ea13770132d98134e14aff0123

hashEnum :: Enum c => c -> Hash
hashEnum e = snd $ foldNatDown apply (const True) (init, defaultSeed) 32
  where
  init :: Hash
  init = fromIntegral $ fromEnum e
  apply :: (Hash, Hash) -> Int -> (Hash, Hash)
  apply (h, acc) pos = (rotateL (xor acc h) 2, acc + rotateL h (4 * pos))
  

hashString :: Text -> RollingHash
hashString txt = RollingHash $ flip (Text.foldl' absorbChar) txt
  where
  absorbChar :: Hash -> Char -> Hash
  absorbChar h c = h `xor` hashEnum c 

hashNum :: Double -> RollingHash
hashNum n = RollingHash $ \h ->
  let (sig, exp) = decodeFloat n in
  (hashEnum sig `xor` shiftL (hashEnum exp) 24)

hashBool :: Bool -> Hash
hashBool True = 1234567890098765432112345678900987654321
hashBool False = 02468642024686420246864202468642024686420

arrConst :: Hash
arrConst = 975319753197531975319753197531975319753197531

objConst :: Hash
objConst = 564634414234564351062456000416554302416346574

hashObj :: (Text, RollingHash) -> RollingHash
hashObj (k, RollingHash vh) =
  let RollingHash hk = hashString k in
  RollingHash $ \h -> rotateL (hk $ rotateR (vh h) 1) 1

instance JSON RollingHash where
  str = hashString
  num = hashNum
  bool b = RollingHash $ \h -> rotateL (h + hashBool b) 8
  null = RollingHash $ flip rotateL 64
  array = concatRollingHashes (RollingHash $ (+ arrConst) . flip rotateR 28)
  obj = concatRollingHashes (RollingHash $ (+ objConst) . flip rotateL 20) . map hashObj

concatRollingHashes :: RollingHash -> [RollingHash] -> RollingHash
concatRollingHashes = foldl concat
  where
  concat (RollingHash f) (RollingHash g) = RollingHash (f . g)

saltedHash :: Hash -> RollingHash -> Hash
saltedHash salt (RollingHash hash) = hash salt

hash :: RollingHash -> Hash
hash = saltedHash 1234567890123456789012345678901234567890
