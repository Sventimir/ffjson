{-# LANGUAGE GADTs, OverloadedStrings, RankNTypes #-}
module Language.Functions (
  Functions(..),
  keysAst,
  arrayMap,
  arrayFilter,
  optionMap,
  numNeg,
  numRecip,
  numPlus,
  numMult,
  minus,
  divide,
  strConcat,
  eq,
  cmp,
  jand,
  jor,
  jnot,
  jIsNull,
  jtry,
  structSize,
  arrayReduce,
  objUnion
) where

import Control.Monad (filterM, foldM, (>=>))
import Control.Monad.Catch (MonadThrow(..))

import Data.Error.Trace (EitherTrace, runEitherTrace)
import Data.Hash (hash)
import Data.JSON (JSON(..))
import Data.JSON.Repr (Repr(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), expectBool,
                      expectNumber, expectString, expectArray, expectObject,
                      toJSON, cmpr)
import qualified Data.Text as Text

class Functions j where
  identity :: j
  compose :: j -> j -> j
  keys :: j -> j
  size :: j -> j
  jmap :: j -> j
  jfilter :: j -> j
  jsum :: j -> j
  jproduct :: j -> j
  jall :: j -> j
  jany :: j -> j
  union :: j -> j -> j
  optMap :: j -> j -> j
  neg :: j -> j
  recipr :: j -> j
  plus :: j -> j -> j
  mult :: j -> j -> j
  concat :: j -> j -> j
  equal :: j -> j -> j
  -- It would be nice perhaps to squash these into a generic compare,
  -- but it's not obvious, how to do it.
  lt :: j -> j -> j
  lte :: j -> j -> j
  gt :: j -> j -> j
  gte :: j -> j -> j
  or :: j -> j -> j
  and :: j -> j -> j
  not :: j -> j
  isNull :: j -> j
  try :: j -> j

  
type JsonF = JsonAst -> EitherTrace JsonAst -- a unary operation on JSON
type JsonF2 = JsonAst -> JsonF              -- a binary operation on JSON


keysAst :: JsonF
keysAst (JObject kvs) = return . array $ map (str . fst) kvs
keysAst json = throwM $ NotAnObject json

structSize :: JsonF
structSize (JArray js) = return . num . toRational $ length js
structSize (JObject js) = return . num . toRational $ length js
structSize (JString s) = return . num . toRational $ Text.length s
structSize j = throwM $ NotSized j

arrayMap :: JsonF -> JsonF
arrayMap f json = do
  items <- expectArray json
  JArray <$> mapM f items

arrayFilter :: JsonF -> JsonF
arrayFilter f json = do
  items <- expectArray json
  JArray <$> filterM (f >=> expectBool) items

arrayReduce :: JsonF2 -> JsonF
arrayReduce f json = do
  items <- expectArray json
  case items of
    [] -> return JNull
    (i : is) -> foldM f i is

objUnion :: JsonF2
objUnion l r = do
  a <- expectObject l
  b <- expectObject r
  return . JObject $ foldr update a b
  where
  update (k, v) [] = [(k, v)]
  update (k', v') ((k, v) : acc)
    | k == k' = (k', v') : acc
    | otherwise = (k, v) : update (k', v') acc

optionMap :: JsonF -> JsonF -> JsonF
optionMap f expr json = do
  j <- expr json
  case j of
    JNull -> return JNull
    _ -> f j

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
numPlus = unitypedBinop expectNumber (+) num

numMult :: JsonF2
numMult = unitypedBinop expectNumber (*) num

minus :: Functions j => j -> j -> j
minus a b = plus a $ neg b

divide :: Functions j => j -> j -> j
divide a b = mult a $ recipr b

strConcat :: JsonF2
strConcat = unitypedBinop expectString Text.append str

-- eq cannot be separated in terms of cmp, because it works on any
-- types, while cmp requires operands' types to match.
eq :: JsonF2
eq l r = return . bool $ hash (toJSON l) == hash (toJSON r)

cmp :: [Ordering] -> JsonF2
cmp ords l r = bool . flip any ords . (==) <$> cmpr l r

jnot :: JsonF
jnot j = JBool . Prelude.not <$> expectBool j

jand :: JsonF2
jand = unitypedBinop expectBool (&&) bool

jor :: JsonF2
jor = unitypedBinop expectBool (||) bool

jIsNull :: JsonF
jIsNull JNull = return $ bool True
jIsNull _ = return $ bool False

jtry :: JsonF -> JsonF
jtry f json = case runEitherTrace $ f json of
  Right j -> return j
  Left _ -> return JNull

unitypedBinop :: (JsonAst -> EitherTrace a) -> (a -> a -> a) -> (a -> JsonAst) ->JsonF2
unitypedBinop checkType op constr l r = do
  a <- checkType l
  b <- checkType r
  return . constr $ op a b


instance Functions (Repr j) where
  identity = Repr $ return "id"
  compose (Repr l) (Repr r) = Repr $ do
    a <- l
    b <- r
    return a <> " | " <> return b
  keys (Repr j) = Repr $ "keys" <> j
  size (Repr j) = Repr $ "size" <> j
  jmap (Repr f) = Repr $ "map (" <> f <> ")"
  jfilter (Repr f) = Repr $ "filter (" <> f <> ")"
  jsum (Repr f) = Repr $ "sum (" <> f <> ")"
  jproduct (Repr f) = Repr $ "product (" <> f <> ")"
  jall (Repr f) = Repr $ "all (" <> f <> ")"
  jany (Repr f) = Repr $ "any (" <> f <> ")"
  union (Repr l) (Repr r) = Repr $ "union (" <> l <> " " <> r <> ")"
  optMap (Repr opt) (Repr f) = Repr $ opt <> " ? " <> f
  neg (Repr j) = Repr $ "neg" <> j
  recipr (Repr j) = Repr $ "recip" <> j
  plus (Repr l) (Repr r) = Repr $ l <> " + " <> r
  mult (Repr l) (Repr r) = Repr $ l <> " * " <> r
  concat (Repr l) (Repr r) = Repr $ l <> " <> " <> r
  equal (Repr l) (Repr r) = Repr $ l <> " = " <> r
  lt (Repr l) (Repr r) = Repr $ l <> " < " <> r
  lte (Repr l) (Repr r) = Repr $ l <> " <= " <> r
  gt (Repr l) (Repr r) = Repr $ l <> " > " <> r
  gte (Repr l) (Repr r) = Repr $ l <> " >= " <> r
  and (Repr l) (Repr r) = Repr $ l <> " && " <> r
  or (Repr l) (Repr r) = Repr $ l <> " || " <> r
  not (Repr j) = Repr $ "not" <> j
  isNull (Repr j) = Repr $ "isNull (" <> j <> ")"
  try (Repr j) = Repr $ "try" <> j
