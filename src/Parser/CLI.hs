{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Parser.CLI (
  CliArgs(..),
  CliError(..),
  parseArgs
) where

import Control.Monad.Except (ExceptT, MonadIO, runExceptT, throwError,
                             liftEither, liftIO)


data CliError = UnrecognisedLong String
              | UnrecognisedShort Char
              | Missing String
              | UserError String
              deriving (Show, Eq)

class CliArgs args where
  defaults :: args
  finalize :: Monad m => args -> ExceptT CliError m args
  positional :: Monad m => args -> String -> ExceptT CliError m args
  hyphens :: Monad m => args -> Int -> ExceptT CliError m args

parseArgs :: (CliArgs args, Monad m) => [String] -> m (Either CliError args)
parseArgs args = runExceptT $ argParser defaults args

argParser :: (CliArgs args, Monad m) => args -> [String] -> ExceptT CliError m args
argParser acc [] = finalize acc
argParser acc (s : ss)
  | stringAll (== '-') s = do
      acc' <- hyphens acc $ length s
      argParser acc' ss
  | otherwise = do
      acc' <- positional acc s
      argParser acc' ss

stringAll :: (Char -> Bool) -> String -> Bool
stringAll pred [] = True
stringAll pred (c : cs)
  | pred c = stringAll pred cs
  | otherwise = False
