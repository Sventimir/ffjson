{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Data.Error.Trace
Description : Provides detailed traces to error messages.
Copyright   : Sventimir 2022

In general, when raising an error in a specialised, small
function, which is being used pervasively around the codebase,
the error message usually lacks information on where the error
happened or the bigger context in which the failure occurred.

This module solves this issue with a wrapper around Either,
whose left type parameter is fixed to a list of exceptions.
These exceptions can contain arbitrary information, helping
localise and describe the error. Typically at each call site
one exception may be added to the trace in order to help
better describe grander context in which the failure took place.

In order for this to work, programmer must take care to wrap
key function calls in @traceError@, which adds specified exceptions
to the trace.
-}
module Data.Error.Trace (
  Trace,
  EitherTrace,
  ExceptTraceT,
  runEitherTrace,
  eitherJustTrace,
  ofEither,
  liftEither,
  liftTrace,
  traceError,
  traceErrorT,
  runExceptTraceT,
  runToIO
) where

import Control.Applicative (Alternative(..))
import Control.Exception (throwIO)
import Control.Monad.Catch (Exception, SomeException(..), MonadThrow(..))
import Control.Monad.Except (ExceptT(..), throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))

import Data.Coerce (coerce)
import Data.List (intercalate)


{- | An exception trace. Currently `Show` instance can be used to display the
     error details to the user. -}
newtype Trace = Trace [SomeException]

instance Semigroup Trace where
  (Trace xs) <> (Trace ys) = Trace (xs <> ys)

instance Monoid Trace where
  mempty = Trace []

instance Show Trace where
  show (Trace es) = "Error trace:\n* "
    <> intercalate "\n* " (show <$> es)

instance Exception a => Exception [a] where

instance Monad m => MonadThrow (ExceptT Trace m) where
  throwM = throwError . singleError
    
infixr 5 !:

(!:) :: Exception e => e -> Trace -> Trace
e !: (Trace es) = Trace (SomeException e : es)

singleError :: Exception e => e -> Trace
singleError e = Trace [SomeException e]

{- | [EitherTrace a] represents either a result of successful computation
     returning an @a@ or an error described by a trace. The trace is a list
     of increasingly specialised exceptions, describing the failure with
     increasing precision. Topmost error message is usually vague and only
     describes the general part of the system where error occurred, while
     further messages narrow the scope of the error and provide more detail
     on what went wrong. This trace needs to be built manually by the programmer
     by annotating computations with possible error details.
-}
newtype EitherTrace a = EitherTrace (Either Trace a)
  deriving (Show)

instance Functor EitherTrace where
  fmap f = EitherTrace . fmap f . coerce

instance Applicative EitherTrace where
  pure = EitherTrace . pure
  (EitherTrace f) <*> (EitherTrace e) = EitherTrace (f <*> e)

instance Monad EitherTrace where
  (EitherTrace (Left e)) >>= _ = EitherTrace $ Left e
  (EitherTrace (Right a)) >>= f = f a

instance MonadThrow EitherTrace where
  throwM e = EitherTrace . Left $ singleError e

{- | Lifts a regular either into the `EitherTrace` context, where a potential
     error is transformed into a trace consisting of just a single exception.
-}
ofEither :: Exception e => Either e a -> EitherTrace a
ofEither (Right a) = EitherTrace $ Right a
ofEither (Left e) = EitherTrace . Left $ singleError e

{- | Takes a `Maybe a` and an exception, and either returns an @a@
     or raises the provided exception.
-}
eitherJustTrace :: Exception e => e -> Maybe a -> EitherTrace a
eitherJustTrace exn Nothing = throwM exn
eitherJustTrace _ (Just a) = return a

{- | @traceError e m@ evaluates @m@ and if it succeeds, returns its result.
     If @m@ fails, however, @e@ is added to the exception trace.
-}
traceError :: Exception e => e -> EitherTrace a -> EitherTrace a
traceError e (EitherTrace (Left (Trace es))) = EitherTrace . Left $ Trace (SomeException e : es)
traceError _ right = right

{- | Escapes the `EitherTrace` @a@ monad and returns either a list of
     exceptions, describing the failure or a result of successful
     computation of type @a@.
-}
runEitherTrace :: EitherTrace a -> Either [SomeException] a
runEitherTrace = coerce


{- | [ExceptTraceT m a] is a monad transformer, where `EitherTrace` @a@ is lifted
     into the context of the @m@ monad.
-}
newtype ExceptTraceT m a = ExceptTraceT (ExceptT Trace m a)

instance Functor m => Functor (ExceptTraceT m) where
  fmap f = ExceptTraceT . fmap f . coerce

instance Monad m => Applicative (ExceptTraceT m) where
  pure = ExceptTraceT . pure
  (ExceptTraceT f) <*> (ExceptTraceT e) = ExceptTraceT (f <*> e)

instance Monad m => Alternative (ExceptTraceT m) where
  empty = ExceptTraceT . ExceptT . return . Left $ Trace []
  (ExceptTraceT ma) <|> (ExceptTraceT mb) = ExceptTraceT (ma <|> mb)

instance Monad m => Monad (ExceptTraceT m) where
  (ExceptTraceT m) >>= f = ExceptTraceT . ExceptT $ do
    res <- runExceptT m
    case res of
      Left es -> return $ Left es
      Right a ->
        let ExceptTraceT m' = f a in
        runExceptT m'

instance MonadIO m => MonadIO (ExceptTraceT m) where
  liftIO = ExceptTraceT . ExceptT . fmap Right . liftIO

instance Monad m => MonadThrow (ExceptTraceT m) where
  throwM = ExceptTraceT . ExceptT . return . Left . singleError

{- | Lifts an `EitherTrace` into the monad transformer context. -}
liftTrace :: Monad m => EitherTrace a -> ExceptTraceT m a
liftTrace = ExceptTraceT . ExceptT . return . coerce

{- | Lifts a regular `Either` into the `ExceptTraceT` transformed,
     transforming a potential left into a trace consisting of just
     a signle expcetion.
-}
liftEither :: (Monad m, Exception e) => Either e a -> ExceptTraceT m a
liftEither (Left e) = ExceptTraceT . ExceptT . return . Left $ singleError e
liftEither (Right a) = ExceptTraceT . ExceptT . return $ Right a

{- | The same as `traceError`, but generalised to the monad transformer
     context.
-}
traceErrorT :: (Monad m, Exception e) => e -> ExceptTraceT m a -> ExceptTraceT m a
traceErrorT e (ExceptTraceT (ExceptT m)) = ExceptTraceT $ ExceptT $ do
  r <- m
  case r of
    Left (Trace es) -> return . Left $ Trace (SomeException e : es)
    right -> return right

{- | @runExceptTraceT m@ evaluates @m@ and returns its result wrapped in the
     inner monad, if it succeeds; otherwise returns the error trace.
-}
runExceptTraceT :: Monad m => ExceptTraceT m a -> m (Either Trace a)
runExceptTraceT = runExceptT . coerce

{- | Escapes the traced computation into any `MonadIO` instance. In case of
     success, result is returned, while in case of error, an IO exception is
     thrown.
-}
runToIO :: MonadIO m => ExceptTraceT m a -> m a
runToIO m = do
  result <- runExceptTraceT m
  case result of
    Right a -> return a
    Left (Trace es) -> liftIO $ throwIO es
