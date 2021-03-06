{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes #-}
module Language.Functions (
  Functions(..),
  keysAst,
  arrayMap,
  numNeg,
  numRecip,
  numPlus,
  numMult,
  minus,
  divide
) where

import Control.Monad.Catch (MonadThrow(..))

import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.Repr (Repr(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), expectNumber)


class Functions j where
  identity :: j
  compose :: j -> j -> j
  keys :: j -> j
  jmap :: j -> j
  neg :: j -> j
  recipr :: j -> j
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

numNeg :: JsonF
numNeg j = do
  n <- expectNumber j
  return $ num (-n)

numRecip :: JsonF
numRecip j = do
  n <- expectNumber j
  if n == 0
    then throwM ZeroDivision
    else return . num $ recip n

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

minus :: Functions j => j -> j -> j
minus a b = plus a $ neg b

divide :: Functions j => j -> j -> j
divide a b = mult a $ recipr b

instance Functions (Repr j) where
  identity = Repr $ return "id"
  compose (Repr l) (Repr r) = Repr $ do
    a <- l
    b <- r
    return a <> " | " <> return b
  keys (Repr j) = Repr $ "keys" <> j
  jmap (Repr f) = Repr $ "map (" <> f <> ")"
  neg (Repr j) = Repr $ "neg" <> j
  recipr (Repr j) = Repr $ "recip" <> j
  plus (Repr l) (Repr r) = Repr $ l <> " + " <> r
  mult (Repr l) (Repr r) = Repr $ l <> " * " <> r
