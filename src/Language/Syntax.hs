{-# LANGUAGE OverloadedStrings #-}
module Language.Syntax (
  Syntax(..),
  getAst,
  indexAst
) where

import Prelude hiding (null)

import Control.Monad.Catch (MonadThrow(..))
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), toJSON)
import Data.JSON.Repr (Repr(..), toText)
import Data.Text (Text)

import Parser.JSON (Parser, punctuation)
import qualified Text.Megaparsec as Megaparsec


class JSON j => Syntax j where
  get :: Text -> j
  index :: Int -> j


getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . maybe null toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

indexAst :: Int -> JsonAst -> EitherTrace JsonAst
indexAst idx (JArray js)
  | idx < 0 = throwM $ NegativeIndex idx
  | otherwise = return $ getIndex idx js
  where
  getIndex :: Int -> [JsonAst] -> JsonAst
  getIndex _ [] = null
  getIndex 0 (x:_) = x
  getIndex i (_:xs) = getIndex (pred i) xs
indexAst _ json = throwM $ NotAnArray json

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


instance Syntax (Repr r) where
  get key = Repr $ return (".\"" <> key <> "\"")
  index i = Repr $ return (".[" <> toText i <> "]")
