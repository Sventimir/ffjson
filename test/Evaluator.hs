{-# LANGUAGE OverloadedStrings #-}
module Evaluator ( evalTests, exprParser ) where

import Prelude hiding (null)
import Test.Hspec
import Test.Hspec

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow(..), SomeException, fromException)

import Data.Error.Trace (ExceptTraceT, liftEither, liftTrace, runToIO)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..))
import Data.Text (Text)

import Language.Eval
import Language.Functions (Functions)
import qualified Language.Functions as Functions
import Language.Syntax (Syntax)
import qualified Language.Syntax as Syntax
import qualified Parser.JSON as JsonParser
import qualified Text.Megaparsec as Megaparsec


evalTests :: Spec
evalTests = do
  describe "Test simple object getter." $ do
    it "Get existing key." $
      (".a" `applyTo` "{\"a\": 1}") `shouldReturn` num 1
    it "Non-existent key returns `null`." $
      (".a" `applyTo` "{}") `shouldReturn` null
    it "Get on non-object term is an error" $
      (".a" `applyTo` "[]") `shouldThrow` notAnObject (array [])
  describe "Object getters compose." $ do
    it "Get from a nested object" $
      (".a.b" `applyTo` "{\"a\": {\"b\": 1234}}") `shouldReturn` num 1234
  describe "Test array getter." $ do
    it "Get from array by existing index." $
      (".[0]" `applyTo` "[1, 2, 3]") `shouldReturn` num 1
    it "Get from array by non-exitent index." $
      (".[0]" `applyTo` "[]") `shouldReturn` null
    it "Array getters compoe." $
      (".[1].[0]" `applyTo` "[0, [1, 2, 3], {}]") `shouldReturn` num 1
    it "Get from non-array fails" $
      (".[0]" `applyTo` "{}") `shouldThrow` notAnArray (obj [])
    it "Get from array by negative index fails." $
      (".[-1]" `applyTo` "[1, 2, 3]") `shouldThrow` negativeIndex (-1)
  describe "Array and objects getters compose together." $
    it "Compose array and object getter." $
      (".a.[0].b" `applyTo` "{\"a\": [{\"b\": true}, 1], \"z\": null}") `shouldReturn` bool True
  describe "Test `keys` function in isolation" $ do
    it "`keys` returns a list a of keys of an object." $
      ("keys" `applyTo` "{}") `shouldReturn` array []
    it "keys returned by `keys` appear in order of definition." $
      ("keys" `applyTo` "{\"a\": 1, \"c\": 3, \"b\": 2}") `shouldReturn` array [str "a", str "c", str "b"]
    it "`keys` applied to non-object fails." $
      ("keys" `applyTo` "[]") `shouldThrow` notAnObject (array [])
  describe "Test filter composition." $ do
    it "Get from keys list." $
      ("keys | .[0]" `applyTo` "{\"aaa\": [], \"zzz\": 12}") `shouldReturn` str "aaa"
  describe "Test parenthesised sub-expressions." $ do
    it "Parentheses enclose expressions." $ 
      ("(keys)" `applyTo` "{}") `shouldReturn` array []
    it "Parentheses separate sub-expressions too." $
      ("(.[0] | .x) | keys" `applyTo` "[{\"x\": {}}]") `shouldReturn` array []
    it "Parentheses can be nested" $
      ("(.x.a) | ((keys) | (.[0]))" `applyTo` "{\"x\": {\"a\": {\"z\": 123}}}")
        `shouldReturn` str "z"
  describe "Test syntax in conjunction with standard JSON." $ do
    it "Complex expressions in dictionary." $
      ("{\"a\": (.x | keys), \"b\": (.y | .[0])}" `applyTo` "{\"x\": {}, \"y\": [1]}")
        `shouldReturn` obj [("a", array []), ("b", num 1)]
  describe "Test addition" $ do
    xit "Add two properties of an object." $
      (".a + .b" `applyTo` "{\"a\": 1, \"b\": 2}") `shouldReturn` num 3

applyTo :: Text -> Text -> IO JsonAst
applyTo exprTxt jsonTxt = runToIO $ do
  json <- liftTrace $ JsonParser.parseJSON jsonTxt
  expr <- liftEither $ Megaparsec.parse exprParser "" exprTxt
  liftTrace $ eval expr json

exprParser :: (Monad m, JSON j, Syntax j, Functions j) => JsonParser.Parser m j
exprParser = Syntax.parser (JsonParser.json exprParser <|> Functions.parser exprParser)


instance (Monad m, JSON a) => JSON (ExceptTraceT m a) where
  str = return . str
  num = return . num
  bool = return . bool
  null = return null
  obj kvs = obj <$> foldM retKVpair [] kvs
  array js = array <$> sequence js

retKVpair :: Monad m => [(a, b)] -> (a, m b) -> m [(a, b)]
retKVpair kvs (k, mv) = do
  v <- mv
  return $ (k, v) : kvs

notAnObject :: JsonAst -> Selector [SomeException]
notAnObject expected [e] = case fromException e of
  Just (NotAnObject actual) -> expected == actual
  Just _ -> False
  Nothing -> False
notAnObject _ _ = False

notAnArray :: JsonAst -> Selector [SomeException]
notAnArray expected [e] = case fromException e of
  Just (NotAnArray actual) -> expected == actual
  Just _ -> False
  Nothing -> False
notAnArray _ _ = False

negativeIndex :: Int -> Selector [SomeException]
negativeIndex expected [e] = case fromException e of
  Just (NegativeIndex i) -> expected == i
  Nothing -> False
negativeIndex _ _ = False
