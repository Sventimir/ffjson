{-# LANGUAGE TupleSections, TypeFamilies #-}
module Language.Eval (
  Eval,
  eval,
) where

import Prelude hiding (null)

import Control.Monad ((>=>))
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst, cmpr)
import Language.Functions (Apply(..), Functions(..))
import qualified Language.Functions as Fun
import Language.Syntax (Syntax(..), getAst, indexAst, sliceAst, ifThenElseAst)


newtype Eval = Eval (JsonAst -> EitherTrace JsonAst)

instance Show Eval where
  show (Eval _) = "<code>"

eval :: Eval -> JsonAst -> EitherTrace JsonAst
eval (Eval f) = f

instance JSON Eval where
  str s = Eval (mconst $ str s)
  num n = Eval (mconst $ num n)
  bool b = Eval (mconst $ bool b)
  null = Eval (mconst null)
  array js = Eval (\j -> array <$> mapM (\(Eval f) -> f j) js)
  obj kvs = Eval (\j -> obj <$> mapM (\(k, Eval f) -> (k, ) <$> f j) kvs)


instance Syntax Eval where
  get key = Eval $ getAst key
  index idx = Eval $ indexAst idx
  slice slc = Eval $ sliceAst slc
  ifThenElse (Eval cond) (Eval ifSo) (Eval ifNot) =
    Eval $ ifThenElseAst cond ifSo ifNot

instance Apply Eval where
  data Fun Eval a r = EvalF (a -> r)
  fun _ = EvalF

instance Functions Eval where
  identity = Eval return
  compose (Eval l) (Eval r) = Eval (l >=> r)
  jmap (Eval f) = Eval $ Fun.arrayMap f
  jfilter (Eval f) = Eval $ Fun.arrayFilter f
  jflatten = uniop Fun.jListFlatten
  jsum = uniop $ Fun.arrayReduce Fun.numPlus
  jproduct = uniop $ Fun.arrayReduce Fun.numMult
  jall = uniop $ Fun.arrayReduce Fun.jand
  jany = uniop $ Fun.arrayReduce Fun.jor
  unique = uniop Fun.jUnique
  union = binop Fun.objUnion
  optMap (Eval opt) (Eval f) = Eval $ Fun.optionMap f opt
  keys = uniop Fun.keysAst
  size = uniop Fun.structSize
  neg = uniop Fun.numNeg
  recipr = uniop Fun.numRecip
  plus = binop Fun.numPlus
  mult = binop Fun.numMult
  concat = binop Fun.strConcat
  equal = binop Fun.eq
  cmp (EvalF f) (Eval l) (Eval r) = Eval $ \j -> do
    a <- l j
    b <- r j
    bool . f <$> cmpr a b
  and = binop Fun.jand
  or = binop Fun.jor
  not = uniop Fun.jnot
  isNull = uniop Fun.jIsNull
  try (Eval j) = Eval $ Fun.jtry j

mconst :: Monad m => a -> b -> m a
mconst = const . return

uniop :: (JsonAst -> EitherTrace JsonAst) -> Eval -> Eval
uniop op (Eval j) = Eval (j >=> op)

binop :: (JsonAst -> JsonAst -> EitherTrace JsonAst) -> Eval -> Eval -> Eval
binop op (Eval l) (Eval r) =
  Eval $ \j -> do
    a <- l j
    b <- r j
    op a b
