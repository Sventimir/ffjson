{-# LANGUAGE FlexibleInstances #-}
module Data.Error.Trace (
  Trace,
  EitherTrace,
  ExceptTraceT,
  singleError,
  runEitherTrace,
  eitherJustTrace,
  ofEither,
  liftEither,
  liftTrace,
  runExceptTraceT,
  runToIO,
  (!:)
) where

import Control.Applicative (Alternative(..))
import Control.Exception (throwIO)
import Control.Monad.Catch (Exception, SomeException(..), MonadThrow(..))
import Control.Monad.Except (ExceptT(..), withExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))

import Data.Coerce (coerce)
import Data.List (intercalate)


newtype Trace = Trace [SomeException]

instance Semigroup Trace where
  (Trace xs) <> (Trace ys) = Trace (xs <> ys)

instance Monoid Trace where
  mempty = Trace []

instance Show Trace where
  show (Trace es) = "Error trace:\n"
    <> (intercalate "\n* " $ fmap show es)

instance Exception a => Exception [a] where

instance Monad m => MonadThrow (ExceptT Trace m) where
  throwM = throwError . singleError
    
infixr 5 !:

(!:) :: Exception e => e -> Trace -> Trace
e !: (Trace es) = Trace (SomeException e : es)

singleError :: Exception e => e -> Trace
singleError e = Trace [SomeException e]

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

ofEither :: Exception e => Either e a -> EitherTrace a
ofEither (Right a) = EitherTrace $ Right a
ofEither (Left e) = EitherTrace . Left $ singleError e

eitherJustTrace :: Exception e => e -> Maybe a -> EitherTrace a
eitherJustTrace exn Nothing = throwM exn
eitherJustTrace _ (Just a) = return a

runEitherTrace :: EitherTrace a -> Either [SomeException] a
runEitherTrace = coerce


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

liftTrace :: Monad m => EitherTrace a -> ExceptTraceT m a
liftTrace = ExceptTraceT . ExceptT . return . coerce

liftEither :: (Monad m, Exception e) => Either e a -> ExceptTraceT m a
liftEither (Left e) = ExceptTraceT . ExceptT . return . Left $ singleError e
liftEither (Right a) = ExceptTraceT . ExceptT . return $ Right a

runExceptTraceT :: Monad m => ExceptTraceT m a -> m (Either [SomeException] a)
runExceptTraceT = runExceptT . withExceptT (\(Trace e) -> e) . coerce

runToIO :: MonadIO m => ExceptTraceT m a -> m a
runToIO m = do
  result <- runExceptTraceT m
  case result of
    Right a -> return a
    Left es -> liftIO $ throwIO es
