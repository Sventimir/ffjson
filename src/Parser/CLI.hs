{-# LANGUAGE GADTs #-}
module Parser.CLI (
  CliArgs(..),
  CliError(..),
  Or(..),
  Consume(..),
  FlagSpec(..),
  parseArgs,
  parseCli,
  argParser,
  cliParser
) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, MonadIO, runExceptT, throwError,
                             liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.List (head, isPrefixOf)
import Data.Word (Word8)

import System.Environment (getArgs)


data CliError = UnrecognisedLong String
              | UnrecognisedShort Char
              | UnexpectedPositional String
              | MissingParam String
              | UserError String
              deriving (Show, Eq)

data Or l r = OrLeft l
            | OrRight r
            | OrBoth l r

data Simple
data Parametric

data Arg a where
  Positional :: String -> Arg Simple
  Dashes :: Int -> Arg Simple
  LongFlag :: String -> Arg Parametric
  ShortFlag :: Char -> Arg Parametric

instance Show (Arg a) where
  show (Positional arg) = arg
  show (Dashes count) = replicate count '-'
  show (LongFlag f) = "--" <> f
  show (ShortFlag f) = ['-', f]

data AnyArg where
  AnyArg :: Arg a -> AnyArg

data Consume m f args where
  ArgS :: Monad m => Consume m f args -> Consume m (String -> f) args
  ArgZ :: Monad m => Consume m (args -> ExceptT CliError m args) args

data FlagSpec m args where
  FlagSpec :: Or Char String -> Consume m f args -> f -> FlagSpec m args

class CliArgs args where
  defaults :: args
  finalize :: Monad m => args -> ExceptT CliError m args
  positional :: Monad m => args -> String -> ExceptT CliError m args
  hyphens :: Monad m => args -> Int -> ExceptT CliError m args
  flags :: Monad m => [FlagSpec m args]
  

parseCli :: (CliArgs args, MonadIO m) => m (Either CliError args)
parseCli = liftIO getArgs >>= parseArgs

parseArgs :: (CliArgs args, Monad m) => [String] -> m (Either CliError args)
parseArgs args = runExceptT $ argParser defaults args

cliParser :: (CliArgs args, MonadIO m) => args -> ExceptT CliError m args
cliParser args = liftIO getArgs >>= argParser args

argParser :: (CliArgs args, Monad m) => args -> [String] -> ExceptT CliError m args
argParser acc strs = concatMapM classifyArg strs >>= consumeArgs acc

consumeArgs :: (CliArgs args, Monad m) => args -> [AnyArg] -> ExceptT CliError m args
consumeArgs acc [] = finalize acc
consumeArgs acc (AnyArg (Positional p) : args) =
  positional acc p >>= flip consumeArgs args
consumeArgs acc (AnyArg (Dashes count) : args) =
  hyphens acc count >>= flip consumeArgs args
consumeArgs acc (AnyArg f@(ShortFlag _) : args) = do
  spec <- findFlag f flags
  (acc', args') <- parseFlag f spec acc args
  consumeArgs acc' args'
consumeArgs acc (AnyArg f@(LongFlag _) : args) = do
  spec <- findFlag f flags
  (acc', args') <- parseFlag f spec acc args
  consumeArgs acc' args'
  
classifyArg :: Monad m => String -> ExceptT CliError m [AnyArg]
classifyArg ['-'] = return [AnyArg $ Dashes 1]
classifyArg ('-' : '-' : flag)
  | all (== '-') flag = return [AnyArg . Dashes $ length flag + 2]
  | otherwise = return [AnyArg $ LongFlag flag]
classifyArg ('-' : flag : []) = return [AnyArg $ ShortFlag flag]
classifyArg flag@('-' : flags) = return $ map (AnyArg . ShortFlag) flags
classifyArg arg = return [AnyArg $ Positional arg]

findFlag :: Monad m => Arg Parametric -> [FlagSpec m args] -> ExceptT CliError m (FlagSpec m args)
findFlag (ShortFlag f) [] = throwError $ UnrecognisedShort f
findFlag (LongFlag f) [] = throwError $ UnrecognisedLong f
findFlag flag@(ShortFlag f) (spec@(FlagSpec (OrLeft m) _ _) : specs)
  | f == m = return spec
  | otherwise = findFlag flag specs
findFlag flag@(ShortFlag f) (spec@(FlagSpec (OrBoth m _) _ _) : specs)
  | f == m = return spec
  | otherwise = findFlag flag specs
findFlag flag@(LongFlag f) (spec@(FlagSpec (OrRight m) _ _) : specs)
  | f == m = return spec
  | otherwise = findFlag flag specs
findFlag flag@(LongFlag f) (spec@(FlagSpec (OrBoth _ m) _ _) : specs)
  | f == m = return spec
  | otherwise = findFlag flag specs
findFlag flag (_ : specs) = findFlag flag specs

parseFlag :: Monad m => Arg Parametric -> FlagSpec m args ->
             args -> [AnyArg] -> ExceptT CliError m (args, [AnyArg])
parseFlag _ (FlagSpec _ ArgZ parse) acc args = parse acc <&> flip (,) args
parseFlag arg (FlagSpec _ (ArgS ty) parse) acc [] = throwError . MissingParam $ show arg
parseFlag arg (FlagSpec f (ArgS ty) parse) acc (AnyArg (Positional p) : args) =
  parseFlag arg (FlagSpec f ty (parse p)) acc args
parseFlag arg (FlagSpec _ (ArgS ty) parse) acc (AnyArg _ : _) =
  throwError . MissingParam $ show arg

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = cmap [] xs
  where
  cmap acc [] = return acc
  cmap acc (x : xs) = do
    ys <- f x
    cmap (acc ++ ys) xs
