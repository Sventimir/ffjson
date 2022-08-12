{-# LANGUAGE GADTs #-}
import Prelude hiding (null)
import Test.Hspec
import Test.QuickCheck
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Error.Trace (runEitherTrace)
import Data.JSON
import Data.JSON.Repr
import Data.Hash
import Parser.JSON

import CLI (cliTests)
import Evaluator (evalTests)

import Debug.Trace


main :: IO ()
main = hspec $ do
  cliTests
  evalTests
  describe "parse-unparse-identity" $ do
    it "parsing after serialisation is an identity" $
      property $ \(Wrapson j) ->
                   let (repr, rollingHash) = j in
                   case runEitherTrace . parseJSON "arbitrary" $ reprS repr defaultReprConfig id of
                     Left _ -> False
                     Right j' -> hash rollingHash == hash j'


data Wrapson a where
  Wrapson :: (Show json, JSON json) => json -> Wrapson json

instance Show (Wrapson json) where
  show (Wrapson j) = show j

instance (Show json, JSON json) => Arbitrary (Wrapson json) where
  arbitrary = oneof [ genString, genNum, genBool, genNull, genArray, genObject ]

genNull, genBool, genNum, genString, genArray, genObject ::
  (Show json, JSON json) => Gen (Wrapson json)
genNull = return $ Wrapson null
genBool = Wrapson . bool <$> chooseAny
-- We rely on generating Doubles, because if we were to generate a fraction having
-- an infinite binary expansion, the test would inevitably fail.
genNum = Wrapson . (num . toRational) <$> (chooseAny :: Gen Double)
genString = Wrapson . str . Text.pack <$> listOf (chooseEnum ('0', 'z'))
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
  key <- Text.pack <$> listOf chooseAny
  value <- arbitrary
  return (key, value)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
