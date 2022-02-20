{-# LANGUAGE GADTs #-}
module Parser.CLI (
  CliArgs(..),
  CliError(..),
  Or(..),
  Consume(..),
  FlagSpec(..),
  parseArgs
) where

import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, MonadIO, runExceptT, throwError,
                             liftEither, liftIO)
import Data.Functor ((<&>))
import Data.List (head, isPrefixOf)


data CliError = UnrecognisedLong String
              | UnrecognisedShort Char
              | UnexpectedPositional String
              | MissingParam String
              | UserError String
              | Impossible
              deriving (Show, Eq)

data Or l r = OrLeft l
            | OrRight r
            | OrBoth l r

data Posit
data Flag a
data Multiple

data Arg a where
  Positional :: String -> Arg Posit
  Dashes :: Int -> Arg Posit
  LongFlag :: String -> Arg (Flag ())
  ShortFlag :: Char -> Arg (Flag ())
  ManyShortFlags :: [Char] -> Arg (Flag Multiple)

data AnyArg where
  AnyArg :: Arg a -> AnyArg

instance Show (Arg a) where
  show (Positional arg) = arg
  show (Dashes count) = replicate count '-'
  show (LongFlag f) = "--" <> f
  show (ShortFlag f) = ['-', f]
  show (ManyShortFlags fs) = '-' : fs

data Consume m f args where
  ConsumeArg :: Monad m => Consume m f args -> Consume m (String -> f) args
  FinalizeArg :: Monad m => Consume m (args -> ExceptT CliError m args) args

data FlagSpec m args where
  FlagSpec :: Or Char String -> Consume m f args -> f -> FlagSpec m args

class CliArgs args where
  defaults :: args
  finalize :: Monad m => args -> ExceptT CliError m args
  positional :: Monad m => args -> String -> ExceptT CliError m args
  hyphens :: Monad m => args -> Int -> ExceptT CliError m args
  flags :: Monad m => [FlagSpec m args]
  

parseArgs :: (CliArgs args, Monad m) => [String] -> m (Either CliError args)
parseArgs args = runExceptT $ argParser defaults args

argParser :: (CliArgs args, Monad m) => args -> [String] -> ExceptT CliError m args
argParser acc [] = finalize acc
argParser acc (s : ss) = do
  AnyArg arg <- classifyArg s
  (acc', ss') <- case arg of
    Positional p -> positional acc p <&> flip (,) ss
    Dashes len -> hyphens acc len <&> flip (,) ss
    ManyShortFlags fs ->
      foldM (flip ($)) (acc, ss) (map interpFlag fs)
    flag@(ShortFlag _) -> do
      spec <- findFlag flag flags
      parseFlag flag spec acc ss
    flag@(LongFlag _) -> do
      spec <- findFlag flag flags
      parseFlag flag spec acc ss
  argParser acc' ss'

classifyArg :: Monad m => String -> ExceptT CliError m AnyArg
classifyArg ['-'] = return . AnyArg $ Dashes 1
classifyArg ('-' : '-' : flag)
  | all (== '-') flag = return . AnyArg . Dashes $ length flag + 2
  | otherwise = return . AnyArg $ LongFlag flag
classifyArg ('-' : flag : []) = return . AnyArg $ ShortFlag flag
classifyArg flag@('-' : flags) = return . AnyArg $ ManyShortFlags flags
classifyArg arg = return . AnyArg $ Positional arg

interpFlag :: (CliArgs args, Monad m) => Char -> (args, [String]) ->
              ExceptT CliError m (args, [String])
interpFlag flag (acc, ss) = do
  spec <- findFlag (ShortFlag flag) flags
  parseFlag (ShortFlag flag) spec acc ss

findFlag :: Monad m => Arg (Flag ()) -> [FlagSpec m args] -> ExceptT CliError m (FlagSpec m args)
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

parseFlag :: Monad m => Arg (Flag ()) -> FlagSpec m args ->
             args -> [String] -> ExceptT CliError m (args, [String])
parseFlag _ (FlagSpec _ FinalizeArg parse) args ss = parse args <&> flip (,) ss
parseFlag arg (FlagSpec _ (ConsumeArg ty) parse) args [] = throwError . MissingParam $ show arg
parseFlag arg (FlagSpec f (ConsumeArg ty) parse) args (s : ss) =
  parseFlag arg (FlagSpec f ty (parse s)) args ss
