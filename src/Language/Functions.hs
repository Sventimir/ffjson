{-# LANGUAGE OverloadedStrings #-}
module Language.Functions (
  Functions(..),
  keysAst,
  parser,
  parse
) where

import Control.Monad.Catch (MonadThrow(..))

import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.Repr (Repr(..))
import Data.JSON.AST (JsonAst(..), TypeError(..))
import Data.Text (Text)

import Parser.JSON (Parser, lexeme)
import qualified Text.Megaparsec as Megaparsec


class JSON j => Functions j where
  keys :: j


keysAst :: JsonAst -> EitherTrace JsonAst
keysAst (JObject kvs) = return . array $ map (str . fst) kvs
keysAst json = throwM $ NotAnObject json


parse :: Functions j => Text -> EitherTrace j
parse = ofEither . Megaparsec.parse parser ""

parser :: (Monad m, Functions j) => Parser m j
parser = objectKeys

objectKeys :: (Monad m, Functions j) => Parser m j
objectKeys = do
  lexeme $ Megaparsec.chunk "keys"
  return keys

instance Functions (Repr j) where
  keys = Repr $ return "keys"
