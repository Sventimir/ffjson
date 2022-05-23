{-# LANGUAGE OverloadedStrings #-}
module Language.Syntax (
  Syntax(..),
  getAst,
  parser,
  parse
) where

import Prelude hiding (null)

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), ObjectError(..), toJSON)
import Data.JSON.Repr (Repr)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Language.Core(Composable(..))

import Parser.JSON (Parser, lexeme)
import Text.Megaparsec (some)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (char, alphaNumChar)


class JSON j => Syntax j where
  get :: Text -> j


getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . fromMaybe null . fmap toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


parse :: (Composable o, Syntax o) => Text -> EitherTrace o
parse = ofEither . Megaparsec.parse parser ""

parser :: (Monad m, Composable o, Syntax o) => Parser m o
parser = getObject


getObject :: (Monad m, Composable o, Syntax o) => Parser m o
getObject = do
  -- the parser below is guaranteed to return a none-empty list.
  e : es <- some $ do
    lexeme $ char '.'
    key <- lexeme $ some alphaNumChar
    return . get $ pack key
  return $ foldl compose e es
  
