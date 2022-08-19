{-# LANGUAGE OverloadedStrings #-}
module Language.Syntax (
  Syntax(..),
  ArraySlice(..),
  getAst,
  indexAst,
  sliceAst,
  ifThenElseAst
) where

import Prelude hiding (null)

import Control.Monad.Catch (MonadThrow(..))
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), toJSON, expectBool)
import Data.JSON.Repr (Repr(..), toText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)


data ArraySlice = ArraySlice {
  start :: Maybe Int,
  end :: Maybe Int,
  step :: Maybe Int
}

class JSON j => Syntax j where
  get :: Text -> j
  index :: Int -> j
  slice :: ArraySlice -> j
  ifThenElse :: j -> j -> j -> j

type JsonF = JsonAst -> EitherTrace JsonAst

getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . maybe null toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

indexAst :: Int -> JsonAst -> EitherTrace JsonAst
indexAst idx (JArray js)
  | idx < 0 = let i = length js + idx in
                return $ if i > 0 then getIndex i js else null
  | otherwise = return $ getIndex idx js
  where
  getIndex :: Int -> [JsonAst] -> JsonAst
  getIndex _ [] = null
  getIndex 0 (x:_) = x
  getIndex i (_:xs) = getIndex (pred i) xs
indexAst _ json = throwM $ NotAnArray json

sliceAst :: ArraySlice -> JsonF
sliceAst slc (JArray js) = do
  let arr = drop (fromMaybe 0 $ getIdx start) $ maybe id take (getIdx end) js
  let s = fromMaybe 1 $ step slc
  case compare s 0 of
    GT -> return . JArray $ takeSlice (s - 1) arr
    EQ -> throwM SliceStepCannotBeZero
    LT -> return . JArray $ takeSlice (abs s - 1) (reverse arr)
  where
  len = length js
  getIdx getter = (\i -> if i < 0 then len + i else i) <$> getter slc
sliceAst _ json = throwM $ NotAnArray json

takeSlice :: Int -> [a] -> [a]
takeSlice _ [] = []
takeSlice s (x : xs) = x : takeSlice s (drop s xs)

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more

ifThenElseAst :: JsonF -> JsonF -> JsonF -> JsonF
ifThenElseAst cond ifSo ifNot j = do
  c <- cond j >>= expectBool
  if c then ifSo j else ifNot j


instance Syntax (Repr r) where
  get key = Repr $ return (".\"" <> key <> "\"")
  index i = Repr $ return (".[" <> toText i <> "]")
  slice slc = Repr $ return (".[" <> showSlc start <> ":" <> showSlc end <> ":" <> showSlc step <> "]")
    where
    showSlc getSlice = maybe "" toText $ getSlice slc
  ifThenElse (Repr cond) (Repr ifSo) (Repr ifNot) =
    Repr $ "if " <> cond <> " then " <> ifSo <> " else " <> ifNot
