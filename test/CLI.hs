module CLI ( cliTests ) where

import Test.Hspec
import Parser.CLI


data Option = Default | OptionA | OptionB | OptionC | OptionD
  deriving (Read, Show, Eq)

newtype StrArgs = StrArgs [String] deriving (Show, Eq)

instance CliArgs StrArgs where
  defaults = StrArgs []
  finalize (StrArgs args) = return . StrArgs $ reverse args
  positional (StrArgs args) = return . StrArgs . (: args)

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


cliTests :: Spec
cliTests = do
  describe "Parse no options at all" $
    it "no options return default configuration" $ do
      result <- parseArgs []
      result `shouldBe` Right (StrArgs [])
  describe "Parse positional String arguments." $ do
    it "a single word" $ do
      result <- parseArgs ["word"]
      result `shouldBe` Right (StrArgs ["word"])
    it "two words are separate arguments" $ do
      result <- parseArgs ["first", "second"]
      result `shouldBe` Right (StrArgs ["first", "second"])
  describe "Parse positional arguments of different types." $ do
    it "pass an enum and a number" $ do
      result <- parseArgs ["optionA", "3.1415"]
      result `shouldBe` Right (OptAndFloat OptionA 3.1415)
    it "order of arguments does not matter here" $ do
      result <- parseArgs ["3.1415", "optionA"]
      result `shouldBe` Right (OptAndFloat OptionA 3.1415)
    it "exceptions raised by client code are not caught" $
      (do
          result <- parseArgs ["ala ma kota"]
          result `shouldBe` Right (defaults :: OptAndFloat))
        `shouldThrow` anyErrorCall
