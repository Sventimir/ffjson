{-# LANGUAGE RankNTypes #-}
module Language.Object (
  Composable(..),
  Object(..),
  getAst,
  parser,
  parse
) where

import Prelude hiding (null)

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), toJSON)
import Data.JSON.Repr (Repr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Parser.JSON (Parser, lexeme)
import qualified Parser.JSON as JsonParser
import Text.Megaparsec (some)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (char, alphaNumChar)


class Composable c where
  compose :: c -> c -> c

class JSON j => Object j where
  get :: Text -> j


data ObjectError = NotAnObject JsonAst

instance Show ObjectError where
  show (NotAnObject j) = "Not an object: '" ++ show (toJSON j :: Repr String) ++ "'!"

instance Exception ObjectError where

getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . fromMaybe null . fmap toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


parse :: (Composable o, Object o) => Text -> EitherTrace o
parse = ofEither . Megaparsec.parse parser ""

parser :: (Monad m, Composable o, Object o) => Parser m o
parser = do
  exprs <- fmap (:[]) (JsonParser.json parser) <|> some getObject
  -- parsers above are guaranteed to return a non-empty list.
  case exprs of
    (e : es) -> return $ foldl compose e es


getObject :: (Monad m, Object o) => Parser m o
getObject = do
  lexeme $ char '.'
  key <- lexeme $ some alphaNumChar
  return . get $ pack key
  
