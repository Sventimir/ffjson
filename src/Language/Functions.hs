{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes #-}
module Language.Functions (
  Functions(..),
  keysAst,
  arrayMap,
  numPlus,
  numMult
) where

import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fix (fix)

import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON (JSON(..))
import Data.JSON.Repr (Repr(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), expectNumber)
import Data.Text (Text)

import Parser.JSON (Parser, lexeme, punctuation)
import qualified Text.Megaparsec as Megaparsec


class Functions j where
  identity :: j
  compose :: j -> j -> j
  keys :: j -> j
  jmap :: j -> j
  plus :: j -> j -> j
  mult :: j -> j -> j


type JsonF = JsonAst -> EitherTrace JsonAst -- a unary operation on JSON
type JsonF2 = JsonAst -> JsonF              -- a binary operation on JSON


keysAst :: JsonF
keysAst (JObject kvs) = return . array $ map (str . fst) kvs
keysAst json = throwM $ NotAnObject json

arrayMap :: JsonF -> JsonF
arrayMap f (JArray items) = JArray <$> mapM f items
arrayMap _ json = throwM $ NotAnArray json

numPlus :: JsonF2
numPlus l r = do
  a <- expectNumber l
  b <- expectNumber r
  return $ num (a + b)

numMult :: JsonF2
numMult l r = do
  a <- expectNumber l
  b <- expectNumber r
  return $ num (a * b)

instance Functions (Repr j) where
  identity = Repr $ return "id"
  compose (Repr l) (Repr r) = Repr $ do
    a <- l
    b <- r
    return a <> " | " <> return b
  keys (Repr j) = Repr $ "keys" <> j
  jmap (Repr f) = Repr $ "map (" <> f <> ")"
  plus (Repr l) (Repr r) = Repr $ l <> " + " <> r
  mult (Repr l) (Repr r) = Repr $ l <> " * " <> r
