{-# LANGUAGE OverloadedStrings #-}
module Language.Syntax (
  Syntax(..),
  getAst,
  indexAst,
  parser
) where

import Prelude hiding (null)

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), toJSON)
import Data.JSON.Repr (Repr(..), toText)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Parser.JSON (Parser, lexeme, punctuation, space)
import Text.Megaparsec (many, some)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


class JSON j => Syntax j where
  compose :: j -> j -> j
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
indexAst _ json = throwM $ NotAList json

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


parser :: (Monad m, Syntax j) => Parser m j -> Parser m j
parser semantic = do
  es <- Megaparsec.sepBy
        (parentheses (parser semantic) <|> getter <|> semantic)
        (punctuation '|')
  case es of
    [] -> fail "Bad syntax"
    e : es -> return $ foldl compose e es

parentheses :: (Monad m, Syntax j) => Parser m j -> Parser m j
parentheses self = Megaparsec.between
                     (punctuation '(')
                     (punctuation ')')
                     self

getter :: (Monad m, Syntax j) => Parser m j
getter = do
  -- the parser below is guaranteed to return a none-empty list.
  e : es <- some (
    Megaparsec.try getArray <|>
    Megaparsec.try quotedGetObject <|>
    getObject)
  return $ foldl compose e es


getObject :: (Monad m, Syntax j) => Parser m j
getObject = lexeme $ do
  char '.'
  key <- some alphaNumChar
  return . get $ pack key

quotedGetObject ::(Monad m, Syntax j) => Parser m j
quotedGetObject = lexeme $ do
  char '.'
  key <- Megaparsec.between (char '"') (punctuation '"')
         . many $ Megaparsec.anySingleBut '"'
  return . get $ pack key
  
getArray :: (Monad m, Syntax j) => Parser m j
getArray = do
  lexeme $ string ".["
  i <- lexeme $ signed space decimal
  lexeme $ char ']'
  return $ index i

instance Syntax (Repr r) where
  compose (Repr l) (Repr r) = Repr $ do
    a <- l
    b <- r
    return a <> " | " <> return b
  get key = Repr $ return (".\"" <> key <> "\"")
  index i = Repr $ return (".[" <> toText i <> "]")
