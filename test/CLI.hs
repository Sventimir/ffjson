module CLI ( cliTests ) where

import Control.Monad.Except (MonadIO, ExceptT, liftIO, throwError)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec
import Parser.CLI


data Option = Default | OptionA | OptionB | OptionC | OptionD
  deriving (Read, Show, Eq)

newtype StrArgs = StrArgs [String] deriving (Show, Eq)

instance CliArgs StrArgs where
  defaults = StrArgs []
  finalize (StrArgs args) = return . StrArgs $ reverse args
  positional (StrArgs args) = return . StrArgs . (: args)
  hyphens (StrArgs args) 1 = return $ StrArgs ("/dev/stdin" : args)
  hyphens _ count =
    throwError $ UserError ("Unrecognized argument: '" <> replicate count '-' <> "'!")
  flags = []

data OptAndFloat = OptAndFloat Option Float
  deriving (Show, Eq)

instance CliArgs OptAndFloat where
  defaults = OptAndFloat Default 0
  finalize = return
  positional (OptAndFloat _ flt) "optionA" = return $ OptAndFloat OptionA flt
  positional (OptAndFloat _ flt) "optionB" = return $ OptAndFloat OptionB flt
  positional (OptAndFloat _ flt) "optionC" = return $ OptAndFloat OptionC flt
  positional (OptAndFloat _ flt) "optionD" = return $ OptAndFloat OptionD flt
  positional (OptAndFloat opt _) flt = return $ OptAndFloat opt (read flt)
  hyphens _ count =
    throwError $ UserError ("Unrecognized argument: '" <> replicate count '-' <> "'!")
  flags = []

newtype ArgMap = ArgMap (Map String String)
  deriving (Show, Eq)

argMap :: [(String, String)] -> Either CliError ArgMap
argMap = Right . ArgMap . Map.fromList

insertArg :: Monad m => String -> String -> ArgMap -> ExceptT CliError m ArgMap
insertArg key val (ArgMap m) = return . ArgMap $ Map.insert key val m

instance CliArgs ArgMap where
  defaults = ArgMap Map.empty
  finalize = return
  positional _ = throwError . UnexpectedPositional
  hyphens _ count = throwError . UnexpectedPositional $ replicate count '-'
  flags = [FlagSpec (OrBoth 'r' "redFlag") FinalizeArg (insertArg "redFlag" "")]


cliTests :: Spec
cliTests = parallel $ do
  describe "Parse no options at all" $
    it "no options return default configuration" $ do
      parseArgs [] `shouldReturn` Right (StrArgs [])
  describe "Parse positional String arguments." $ do
    it "a single word" $ do
      parseArgs ["word"]`shouldReturn` Right (StrArgs ["word"])
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
  describe "Parse hyphen as a special argument" $ do
    it "hyphen is interpreted as stdin" $ do
      parseArgs ["-"] `shouldReturn` Right (StrArgs ["/dev/stdin"])
  describe "Test parsing named arguments" $ do
    it "pass no arguments at all" $ do
      parseArgs [] `shouldReturn` Right (defaults :: ArgMap)
    it "Test a single short flag with no arguments" $
      parseArgs ["-r"] `shouldReturn` argMap [("redFlag", "")]

