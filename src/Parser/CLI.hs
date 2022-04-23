{-# LANGUAGE FunctionalDependencies, GADTs, MultiParamTypeClasses #-}
module Parser.CLI (
  CliArgs(..),
  CliError(..),
  Or(..),
  Consume(..),
  FlagSpec(..),
  argParser,
  cliParser
) where

import Control.Monad (foldM)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT, MonadIO, runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.List (head, isPrefixOf)
import Data.Word (Word8)

import System.Environment (getArgs)


data CliError = UnrecognisedLong String
              | UnrecognisedShort Char
              | MissingParam String
              deriving (Eq, Show)

instance Exception CliError where

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
  ArgS :: MonadThrow m => Consume m f args -> Consume m (String -> f) args
  ArgZ :: MonadThrow m => Consume m (args -> m args) args

data FlagSpec m args where
  FlagSpec :: Or Char String -> Consume m f args -> f -> FlagSpec m args

class MonadThrow m => CliArgs m args | args -> m where
  defaults :: args
  finalize :: args -> m args
  positional :: args -> String -> m args
  hyphens :: args -> Int -> m args
  flags :: [FlagSpec m args]
  

cliParser :: (CliArgs m args, MonadIO m) => args -> m args
cliParser args = liftIO getArgs >>= argParser args

argParser :: CliArgs m args => args -> [String] -> m args
argParser acc strs = concatMapM classifyArg strs >>= consumeArgs acc

consumeArgs :: CliArgs m args => args -> [AnyArg] -> m args
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
  
classifyArg :: MonadThrow m => String -> m [AnyArg]
classifyArg ['-'] = return [AnyArg $ Dashes 1]
classifyArg ('-' : '-' : flag)
  | all (== '-') flag = return [AnyArg . Dashes $ length flag + 2]
  | otherwise = return [AnyArg $ LongFlag flag]
classifyArg ('-' : flag : []) = return [AnyArg $ ShortFlag flag]
classifyArg flag@('-' : flags) = return $ map (AnyArg . ShortFlag) flags
classifyArg arg = return [AnyArg $ Positional arg]

findFlag :: MonadThrow m => Arg Parametric -> [FlagSpec m args] -> m (FlagSpec m args)
findFlag (ShortFlag f) [] = throwM $ UnrecognisedShort f
findFlag (LongFlag f) [] = throwM $ UnrecognisedLong f
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

parseFlag :: MonadThrow m => Arg Parametric -> FlagSpec m args ->
             args -> [AnyArg] -> m (args, [AnyArg])
parseFlag _ (FlagSpec _ ArgZ parse) acc args = parse acc <&> flip (,) args
parseFlag arg (FlagSpec _ (ArgS ty) parse) acc [] = throwM . MissingParam $ show arg
parseFlag arg (FlagSpec f (ArgS ty) parse) acc (AnyArg (Positional p) : args) =
  parseFlag arg (FlagSpec f ty (parse p)) acc args
parseFlag arg (FlagSpec f (ArgS ty) parse) acc (AnyArg (Dashes len) : args) =
  parseFlag arg (FlagSpec f ty (parse $ replicate len '-')) acc args
parseFlag arg (FlagSpec _ (ArgS ty) parse) acc (AnyArg _ : _) =
  throwM . MissingParam $ show arg

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = cmap [] xs
  where
  cmap acc [] = return acc
  cmap acc (x : xs) = do
    ys <- f x
    cmap (acc ++ ys) xs
