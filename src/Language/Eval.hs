{-# LANGUAGE TupleSections #-}
module Language.Eval (
  Eval,
  eval,
) where

import Prelude hiding (null)

import Control.Monad ((>=>))
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst)
import Language.Functions (Functions(..))
import qualified Language.Functions as Fun
import Language.Syntax (Syntax(..), getAst, indexAst)


newtype Eval = Eval (JsonAst -> EitherTrace JsonAst)

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
  compose (Eval l) (Eval r) = Eval (l >=> r)
  get key = Eval $ getAst key
  index idx = Eval $ indexAst idx

instance Functions Eval where
  keys = Eval Fun.keysAst
  jmap (Eval f) = Eval $ Fun.arrayMap f
  plus (Eval l) (Eval r) =
    Eval $ \j -> do
      a <- l j
      b <- r j
      Fun.numPlus a b

mconst :: Monad m => a -> b -> m a
mconst = const . return
