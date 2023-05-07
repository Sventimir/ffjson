{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.Functions
  ( Apply(..)
  , Functions(..)
  , keysAst
  , arrayMap
  , arrayFilter
  , optionMap
  , numNeg
  , numRecip
  , numPlus
  , numMult
  , minus
  , divide
  , strConcat
  , eq
  , lt
  , gt
  , lte
  , gte
  , jand
  , jor
  , jnot
  , jIsNull
  , jDefault
  , jUnique
  , jListFlatten
  , jtry
  , structSize
  , arrayReduce
  , objUnion
  ) where

import Control.Monad (filterM, foldM, (>=>))
import Control.Monad.Catch (MonadThrow(..))

import Data.Error.Trace (EitherTrace, runEitherTrace)
import Data.Hash (hash)
import Data.JSON (JSON(..))
import Data.JSON.Repr (Repr(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..), expectBool,
                      expectNumber, expectString, expectArray, expectObject,
                      toJSON)
import qualified Data.Text as Text

import Language.Syntax (Syntax(..))


class Apply a where
  data Fun a arg ret
  fun :: Text.Text -> (arg -> ret) -> Fun a arg ret

class (Syntax j, Apply j) => Functions j where
  identity :: j
  compose :: j -> j -> j
  keys :: j -> j
  size :: j -> j
  jmap :: j -> j
  jfilter :: j -> j
  jflatten :: j -> j
  jsum :: j -> j
  jproduct :: j -> j
  jall :: j -> j
  jany :: j -> j
  unique :: j -> j
  union :: j -> j -> j
  optMap :: j -> j -> j
  neg :: j -> j
  recipr :: j -> j
  plus :: j -> j -> j
  mult :: j -> j -> j
  concat :: j -> j -> j
  equal :: j -> j -> j
  cmp :: (Fun j Ordering Bool) -> j -> j -> j
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

jnot :: JsonF
jnot j = JBool . Prelude.not <$> expectBool j

jand :: JsonF2
jand = unitypedBinop expectBool (&&) bool

jor :: JsonF2
jor = unitypedBinop expectBool (||) bool

jIsNull :: JsonF
jIsNull JNull = return $ bool True
jIsNull _ = return $ bool False

jDefault :: JsonF2
jDefault dflt JNull = return dflt
jDefault _ v = return v

jListFlatten :: JsonF
jListFlatten j = do
  js <- expectArray j
  doFlatten [] js
  where
  doFlatten acc [] = return $ array acc
  doFlatten acc (JArray js : js') = doFlatten (acc ++ js) js'
  doFlatten acc (j' : js) = doFlatten (acc ++ [j']) js

jUnique :: JsonF
jUnique input = do
  js <- expectArray input
  return . array $ filterUnique [] js
  where
  filterUnique acc [] = reverse acc
  filterUnique acc (j : js) =
    let h = hash $ toJSON j in
    if any (\j' -> hash (toJSON j') == h) acc
    then filterUnique acc js
    else filterUnique (j : acc) js
  

jtry :: JsonF -> JsonF
jtry f json = case runEitherTrace $ f json of
  Right j -> return j
  Left _ -> return JNull

-- Equal does not fail on type mismatch, but returns false instead.
lt, lte, gt, gte :: (JSON j, Functions j) => j -> j -> j
lt = cmp $ fun "<" (== LT)
gt = cmp $ fun ">" (== GT)
lte = cmp $ fun "<=" (/= GT)
gte = cmp $ fun ">=" (/= LT)

unitypedBinop :: (JsonAst -> EitherTrace a) -> (a -> a -> a) -> (a -> JsonAst) ->JsonF2
unitypedBinop checkType op constr l r = do
  a <- checkType l
  b <- checkType r
  return . constr $ op a b

instance Apply (Repr j) where
  data Fun (Repr j) a r = ReprF Text.Text (a -> r)
  fun = ReprF

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
  jflatten (Repr f) = Repr $ "flatten (" <> f <> ")"
  jsum (Repr f) = Repr $ "sum (" <> f <> ")"
  jproduct (Repr f) = Repr $ "product (" <> f <> ")"
  jall (Repr f) = Repr $ "all (" <> f <> ")"
  jany (Repr f) = Repr $ "any (" <> f <> ")"
  unique (Repr f) = Repr $ "unique (" <> f <> ")"
  union (Repr l) (Repr r) = Repr $ "union (" <> l <> " " <> r <> ")"
  optMap (Repr opt) (Repr f) = Repr $ opt <> " ? " <> f
  neg (Repr j) = Repr $ "neg" <> j
  recipr (Repr j) = Repr $ "recip" <> j
  plus (Repr l) (Repr r) = Repr $ l <> " + " <> r
  mult (Repr l) (Repr r) = Repr $ l <> " * " <> r
  concat (Repr l) (Repr r) = Repr $ l <> " <> " <> r
  equal (Repr l) (Repr r) = Repr $ l <> " = " <> r
  cmp (ReprF n _) (Repr l) (Repr r) = Repr $ "compare(" <> return n
                                          <> l <> ") (" <> r <> ")"
  and (Repr l) (Repr r) = Repr $ l <> " && " <> r
  or (Repr l) (Repr r) = Repr $ l <> " || " <> r
  not (Repr j) = Repr $ "not" <> j
  isNull (Repr j) = Repr $ "isNull (" <> j <> ")"
  try (Repr j) = Repr $ "try" <> j
