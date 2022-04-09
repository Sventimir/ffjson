module Data.Error.Trace (
  ETrace,
  TracedEither,
  TracedExceptT,
  singleError,
  addLeft,
  throwLeft,
  ofEither,
  addError,
  throw,
  ofExceptT
) where

import Control.Monad.Except (ExceptT, withExceptT, throwError)
import Data.List (intercalate)


newtype ETrace e = ETrace [e]

instance Semigroup (ETrace a) where
  (ETrace xs) <> (ETrace ys) = ETrace (xs <> ys)

instance Monoid (ETrace a) where
  mempty = ETrace []

instance Functor ETrace where
  fmap f (ETrace es) = ETrace $ fmap f es

instance Show a => Show (ETrace a) where
  show (ETrace es) = "Error trace:\n"
    <> (intercalate "\n* " $ fmap show es)
    
infixr 5 !:

(!:) :: e -> ETrace e -> ETrace e
e !: (ETrace es) = ETrace (e : es)

singleError :: e -> ETrace e
singleError e = ETrace [e]

type TracedExceptT e m a = ExceptT (ETrace e) m a

type TracedEither e a = Either (ETrace e) a

throwLeft :: e -> TracedEither e a
throwLeft = Left . (!: mempty)

addLeft :: e -> TracedEither e a -> TracedEither e a
addLeft _ ok@(Right _) = ok
addLeft e (Left es) = Left $ (e !: es)

ofEither :: Either e a -> TracedEither e a
ofEither (Right a) = Right a
ofEither (Left e) = Left $ singleError e

throw :: Monad m => e -> TracedExceptT e m a
throw = throwError . singleError

addError :: Monad m => e -> TracedExceptT e m a -> TracedExceptT e m a
addError e = withExceptT (e !:)

ofExceptT :: Monad m => ExceptT e m a -> TracedExceptT e m a
ofExceptT = withExceptT singleError
