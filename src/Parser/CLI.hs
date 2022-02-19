{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Parser.CLI (
  CliArgs(..),
  CliError(..),
  parseArgs
) where

import Control.Exception (try)
import Control.Monad.Except (ExceptT, MonadIO, runExceptT, throwError,
                             liftEither, liftIO)


data CliError = UnrecognisedLong String
              | UnrecognisedShort Char
              | Missing String
              | UserError String
              deriving (Show, Eq)

class CliArgs args where
  defaults :: args
  finalize :: MonadIO m => args -> ExceptT CliError m args
  positional :: MonadIO m => args -> String -> ExceptT CliError m args

parseArgs :: (CliArgs args, MonadIO m) => [String] -> m (Either CliError args)
parseArgs args = runExceptT $ argParser defaults args

argParser :: (CliArgs args, MonadIO m) => args -> [String] -> ExceptT CliError m args
argParser acc [] = finalize acc
argParser acc (s : ss) = do
  acc' <- positional acc s
  argParser acc' ss
