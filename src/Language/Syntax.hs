{-# LANGUAGE OverloadedStrings #-}
module Language.Syntax (
  Syntax(..),
  getAst,
  indexAst,
  parser,
  parse
) where

import Prelude hiding (null)

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), toJSON)
import Data.JSON.Repr (Repr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Language.Core(Composable(..))

import Parser.JSON (Parser, lexeme)
import Text.Megaparsec (some)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal)


class JSON j => Syntax j where
  get :: Text -> j
  index :: Int -> j


getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . fromMaybe null . fmap toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

indexAst :: Int -> JsonAst -> EitherTrace JsonAst
indexAst index (JArray js)
  | index < 0 = throwM $ NegativeIndex index
  | otherwise = return $ getIndex index js
  where
  getIndex :: Int -> [JsonAst] -> JsonAst
  getIndex _ [] = null
  getIndex 0 (x:_) = x
  getIndex i (_:xs) = getIndex (pred i) xs

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


parse :: (Composable j, Syntax j) => Text -> EitherTrace j
parse = ofEither . Megaparsec.parse parser ""

parser :: (Monad m, Composable j, Syntax j) => Parser m j
parser = do
  -- the parser below is guaranteed to return a none-empty list.
  e : es <- some (Megaparsec.try getArray <|> getObject)
  return $ foldl compose e es


getObject :: (Monad m, Syntax j) => Parser m j
getObject = do
  lexeme $ char '.'
  key <- lexeme $ some alphaNumChar
  return . get $ pack key
  
getArray :: (Monad m, Syntax j) => Parser m j
getArray = do
  lexeme $ string ".["
  i <- lexeme decimal
  lexeme $ char ']'
  return $ index i
