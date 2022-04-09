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

import Control.Monad.Catch (Exception, SomeException(..))
import Control.Monad.Except (ExceptT, withExceptT, throwError)
import Data.List (intercalate)


newtype ETrace = ETrace [SomeException]

instance Semigroup ETrace where
  (ETrace xs) <> (ETrace ys) = ETrace (xs <> ys)

instance Monoid ETrace where
  mempty = ETrace []

instance Show ETrace where
  show (ETrace es) = "Error trace:\n"
    <> (intercalate "\n* " $ fmap show es)
    
infixr 5 !:

(!:) :: Exception e => e -> ETrace -> ETrace
e !: (ETrace es) = ETrace (SomeException e : es)

singleError :: Exception e => e -> ETrace
singleError e = ETrace [SomeException e]

type TracedExceptT m a = ExceptT ETrace m a

type TracedEither a = Either ETrace a

throwLeft :: Exception e => e -> TracedEither a
throwLeft = Left . (!: mempty)

addLeft :: Exception e => e -> TracedEither a -> TracedEither a
addLeft _ ok@(Right _) = ok
addLeft e (Left es) = Left $ (e !: es)

ofEither :: Exception e => Either e a -> TracedEither a
ofEither (Right a) = Right a
ofEither (Left e) = Left $ singleError e

throw :: (Exception e, Monad m) => e -> TracedExceptT m a
throw = throwError . singleError

addError :: (Exception e, Monad m) => e -> TracedExceptT m a -> TracedExceptT m a
addError e = withExceptT (e !:)

ofExceptT :: (Exception e, Monad m) => ExceptT e m a -> TracedExceptT m a
ofExceptT = withExceptT singleError
