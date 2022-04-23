{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module CLI ( cliTests ) where

import Control.Monad.Except (MonadIO, Except, ExceptT, liftIO, throwError,
                             runExceptT, withExceptT)
import Control.Monad.Catch (MonadThrow(..), Exception, SomeException(..),
                           fromException)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec
import Parser.CLI


data Option = Default | OptionA | OptionB | OptionC | OptionD
  deriving (Read, Show, Eq)

newtype StrArgs = StrArgs [String] deriving (Show, Eq)

data Error = UnexpectedPositional String
           | UserError String
           deriving (Eq, Show)

instance Exception Error where

instance CliArgs (ExceptT SomeException IO) StrArgs where
  defaults = StrArgs []
  finalize (StrArgs args) = return . StrArgs $ reverse args
  positional (StrArgs args) = return . StrArgs . (: args)
  hyphens (StrArgs args) 1 = return $ StrArgs ("/dev/stdin" : args)
  hyphens _ count =
    throwM $ UserError ("Unrecognized argument: '" <> replicate count '-' <> "'!")
  flags = []

data OptAndFloat = OptAndFloat Option Float
  deriving (Show, Eq)

instance CliArgs (ExceptT SomeException IO) OptAndFloat where
  defaults = OptAndFloat Default 0
  finalize = return
  positional (OptAndFloat _ flt) "optionA" = return $ OptAndFloat OptionA flt
  positional (OptAndFloat _ flt) "optionB" = return $ OptAndFloat OptionB flt
  positional (OptAndFloat _ flt) "optionC" = return $ OptAndFloat OptionC flt
  positional (OptAndFloat _ flt) "optionD" = return $ OptAndFloat OptionD flt
  positional (OptAndFloat opt _) flt = return $ OptAndFloat opt (read flt)
  hyphens _ count =
    throwM $ UserError ("Unrecognized argument: '" <> replicate count '-' <> "'!")
  flags = []

newtype ArgMap = ArgMap (Map String String)
  deriving (Show, Eq)

argMap :: [(String, String)] -> Either e ArgMap
argMap = Right . ArgMap . Map.fromList

insertArg :: Monad m => String -> String -> ArgMap -> ExceptT SomeException m ArgMap
insertArg key val (ArgMap m) = return . ArgMap $ Map.insert key val m

instance CliArgs (ExceptT SomeException IO) ArgMap where
  defaults = ArgMap Map.empty
  finalize = return
  positional _ = throwM . UnexpectedPositional
  hyphens _ count = throwM . UnexpectedPositional $ replicate count '-'
  flags = [
      FlagSpec (OrBoth 'w' "write") ArgZ (insertArg "write" ""),
      FlagSpec (OrBoth 'r' "read") ArgZ (insertArg "read" ""),
      FlagSpec (OrBoth 'o' "output") (ArgS ArgZ) (insertArg "output"),
      FlagSpec
        (OrRight "fromTo")
        (ArgS $ ArgS ArgZ)
        (\a b -> insertArg "fromTo" (a <> " -> " <> b))
    ]

type ParseResult a = Either String a

parseArgs :: CliArgs (ExceptT SomeException IO) a => [String] -> IO (ParseResult a)
parseArgs args = runExceptT . withExceptT show $ argParser defaults args

unrecognisedShort :: Char -> Selector SomeException
unrecognisedShort c exc = case fromException exc :: Maybe CliError of
  Just (UnrecognisedShort f) -> c == f
  _ -> False

unexpectedPositional :: String -> Selector SomeException
unexpectedPositional str exc = case fromException exc :: Maybe Error of
  Just (UnexpectedPositional p) -> str == p
  _ -> False

missingParam :: String -> Selector SomeException
missingParam str exc = case fromException exc :: Maybe CliError of
  Just (MissingParam p) -> str == p
  _ -> False


cliTests :: Spec
cliTests = parallel $ do
  describe "Parse no options at all" $
    it "no options return default configuration" $ do
      parseArgs [] `shouldReturn` Right (StrArgs [])
  describe "Parse positional String arguments." $ do
    it "a single word" $ do
      parseArgs ["word"] `shouldReturn` Right (StrArgs ["word"])
    it "two words are separate arguments" $ do
      parseArgs ["first", "second"]
        `shouldReturn` Right (StrArgs ["first", "second"])
  describe "Parse positional arguments of different types." $ do
    it "pass an enum and a number" $ do
      parseArgs ["optionA", "3.1415"]
        `shouldReturn` Right (OptAndFloat OptionA 3.1415)
    it "order of arguments does not matter here" $ do
      parseArgs ["3.1415", "optionA"]
        `shouldReturn` Right (OptAndFloat OptionA 3.1415)
    it "exceptions raised by client code are not caught" $
      (do
          parseArgs ["ala ma kota"]
            `shouldReturn` Right (defaults :: OptAndFloat))
        `shouldThrow` anyErrorCall
  describe "Parse hyphen as a special argument." $ do
    it "hyphen is interpreted as stdin." $ do
      parseArgs ["-"] `shouldReturn` Right (StrArgs ["/dev/stdin"])
  describe "Test parsing single named arguments." $ do
    it "pass no arguments at all." $ do
      parseArgs [] `shouldReturn` Right (defaults :: ArgMap)
    it "Test a single short flag with no arguments." $
      parseArgs ["-r"] `shouldReturn` argMap [("read", "")]
    it "Test a single long flag with no arguments." $
      parseArgs ["--read"] `shouldReturn` argMap [("read", "")]
    it "Long flags prefixed with single dash aren't accepted." $
      (parseArgs ["-read"] :: IO (ParseResult ArgMap))
        `shouldThrow` unrecognisedShort 'e'
    it "Further arguments after a flag are considered positional." $
      (parseArgs ["-r", "ala", "ma", "kota"] :: IO (ParseResult ArgMap))
        `shouldThrow` unexpectedPositional "ala"
  describe "Parse a single named argument with parameters." $ do
    it "With one parameter to a flag, one more argument is consumed." $
      parseArgs ["-o", "/dev/stdout"]
        `shouldReturn` argMap [("output", "/dev/stdout")]
    it "If the parameter is missing, it's considered an error." $
      (parseArgs ["--output"] :: IO (ParseResult ArgMap))
        `shouldThrow` missingParam "--output"
    it "Further arguments are parsed normally." $
      (parseArgs ["-o", "/dev/stdout", "xxx"] :: IO (ParseResult ArgMap))
        `shouldThrow` unexpectedPositional "xxx"
  describe "Parse a single named argument with two params." $
    it "Two-parameter flag consumes 2 arguments." $
      parseArgs ["--fromTo", "a", "b"] `shouldReturn` argMap [("fromTo", "a -> b")]
  describe "Parse multiple named arguments." $ do
    it "All are gathered into a map." $
      parseArgs ["-o", "/dev/stdout", "-r", "--fromTo", "A", "B"]
        `shouldReturn` argMap [("output", "/dev/stdout"), ("fromTo", "A -> B"), ("read", "")]
    it "Order does not matter." $
      parseArgs ["--fromTo", "A", "B", "-o", "/dev/stdout", "-r"]
        `shouldReturn` argMap [("output", "/dev/stdout"), ("fromTo", "A -> B"), ("read", "")]
    it "Flag param may not start with a dash." $
      (parseArgs ["-o", "-r"] :: IO (ParseResult ArgMap)) `shouldThrow` missingParam "-o"
  describe "It's possible to introduce several short flags with single dash" $ do
    it "Parameters for the last such flag follow." $
      parseArgs ["-rw"] `shouldReturn` argMap [("read", ""), ("write", "")]
    it "Last short flag in sequence can consume further arguments." $
      parseArgs ["-wo", "/dev/stdout"]
        `shouldReturn` argMap [("output", "/dev/stdout"), ("write", "")]
    it "Non-last short flag can't consume parameters." $
      (parseArgs ["-ow", "/dev/stdout"] :: IO (ParseResult ArgMap))
        `shouldThrow` missingParam "-o"
      

