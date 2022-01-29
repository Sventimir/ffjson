{-# LANGUAGE GADTs #-}
import Prelude hiding (null)
import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as Text

import Data.JSON
import Data.JSON.Repr
import Data.Hash
import Parser.JSON


main :: IO ()
main = hspec $ do
  describe "parse-unparse-identity" $
    it "parsing after serialisation is an identity" $
      property $ \(Wrapson json) ->
                   let (repr, rollingHash) = json in
                   case parseJSON $ text repr of
                     Left _ -> False
                     Right json' -> hash rollingHash == hash json'


data Wrapson a where
  Wrapson :: (Show json, JSON json) => json -> Wrapson json

instance Show (Wrapson json) where
  show (Wrapson j) = show j

instance (Show json, JSON json) => Arbitrary (Wrapson json) where
  arbitrary = oneof [ genString, genNum, genBool, genNull, genArray, genObject ]

genNull, genBool, genNum, genString, genArray, genObject ::
  (Show json, JSON json) => Gen (Wrapson json)
genNull = return $ Wrapson null
genBool = fmap (Wrapson . bool) chooseAny
genNum = fmap (Wrapson . num) chooseAny
genString = fmap (Wrapson . str . Text.pack) $ listOf chooseAny
genArray = do
  s <- chooseInt (0, 10)
  fmap (Wrapson . array . map fromWrapson) . resize s $ listOf arbitrary
genObject = do
  s <- chooseInt (0, 10)
  fmap (Wrapson . obj . map (mapSnd fromWrapson)) . resize s $ listOf keyValuePair

fromWrapson :: Wrapson j -> j
fromWrapson (Wrapson j) = j

keyValuePair :: (Show json, JSON json) => Gen (Text, Wrapson json)
keyValuePair = do
  key <- fmap Text.pack $ listOf chooseAny
  value <- arbitrary
  return (key, value)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
