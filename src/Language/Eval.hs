module Language.Eval (
  Eval,
  Composable(..),
  eval,
) where

import Prelude hiding (null)

import Control.Monad ((>=>))
import Control.Monad.Catch (Exception)
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst, toJSON)
import Language.Core (Composable(..))
import Language.Syntax (Syntax(..), getAst, keysAst)
import Data.JSON.Repr (Repr)


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


instance Syntax Eval where
  get key = Eval $ getAst key
  keys = Eval keysAst

instance Composable Eval where
  compose (Eval l) (Eval r) = Eval (l >=> r)

mconst :: Monad m => a -> b -> m a
mconst = const . return
