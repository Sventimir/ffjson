{-# LANGUAGE RankNTypes #-}
module Language.Object (
  Object(..),
  Eval,
  compose,
  eval,
  parser,
  parse
) where

import Prelude hiding (null)

import Control.Applicative (Alternative((<|>)))
import Control.Monad ((>=>))
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

class JSON j => Object j where
  get :: Text -> j

data EvalError = NotAnObject JsonAst

instance Show EvalError where
  show (NotAnObject j) = "Not an object: '" ++ show (toJSON j :: Repr String) ++ "'!"

instance Exception EvalError where


newtype Eval = Eval (JsonAst -> EitherTrace JsonAst)

eval :: Eval -> JsonAst -> EitherTrace JsonAst
eval (Eval f) json = f json

instance JSON Eval where
  str s = Eval (mconst $ str s)
  num n = Eval (mconst $ num n)
  bool b = Eval (mconst $ bool b)
  null = Eval (mconst null)
  array js = Eval (\j -> fmap array $ mapM (\(Eval f) -> f j) js)
  obj kvs = Eval (\j -> fmap obj $ mapM (\(k, Eval f) -> fmap ((,) k) $ f j) kvs)


instance Object Eval where
  get key = Eval $ getAst key

compose :: Eval -> Eval -> Eval
compose (Eval l) (Eval r) = Eval (l >=> r)

getAst :: Text -> JsonAst -> EitherTrace JsonAst
getAst key (JObject kvs) = return . fromMaybe null . fmap toJSON $ find key kvs
getAst _ json = throwM $ NotAnObject json

mconst :: Monad m => a -> b -> m a
mconst = const . return

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find key ((k, v) : more)
  | k == key = Just v
  | otherwise = find key more


parse :: Object o => Text -> EitherTrace o
parse = ofEither . Megaparsec.parse parser ""

parser :: (Monad m, Object o) => Parser m o
parser = JsonParser.json <|> getObject


getObject :: (Monad m, Object o) => Parser m o
getObject = do
  lexeme $ char '.'
  key <- lexeme $ some alphaNumChar
  return . get $ pack key
  
