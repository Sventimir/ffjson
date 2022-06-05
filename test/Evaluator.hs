{-# LANGUAGE OverloadedStrings #-}
module Evaluator ( evalTests ) where

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
    it "Get from non-array fails" $
      (".[0]" `applyTo` "{}") `shouldThrow` notAnArray (obj [])
    it "Get from array by negative index fails." $
      (".[-1]" `applyTo` "[1, 2, 3]") `shouldThrow` negativeIndex (-1)

applyTo :: Text -> Text -> IO JsonAst
applyTo exprTxt jsonTxt = runToIO $ do
  json <- liftTrace $ JsonParser.parseJSON jsonTxt
  expr <- liftEither $ Megaparsec.parse exprParser "" exprTxt
  liftTrace $ eval expr json

exprParser :: (Monad m, JSON j, Syntax j, Functions j) => JsonParser.Parser m j
exprParser = JsonParser.json exprParser
             <|> Syntax.parser exprParser
             <|> Functions.parser


instance (Monad m, JSON a) => JSON (ExceptTraceT m a) where
  str = return . str
  num = return . num
  bool = return . bool
  null = return null
  obj kvs = fmap obj $ foldM retKVpair [] kvs
  array js = fmap array $ sequence js

retKVpair :: Monad m => [(a, b)] -> (a, m b) -> m [(a, b)]
retKVpair kvs (k, mv) = do
  v <- mv
  return $ (k, v) : kvs

notAnObject :: JsonAst -> Selector [SomeException]
notAnObject expected [e] = case fromException e of
  Just (NotAnObject actual) -> expected == actual
  Nothing -> False
notAnObject _ _ = False

notAnArray :: JsonAst -> Selector [SomeException]
notAnArray expected [e] = case fromException e of
  Just (NotAList actual) -> expected == actual
  Nothing -> False
notAnArray _ _ = False

negativeIndex :: Int -> Selector [SomeException]
negativeIndex expected [e] = case fromException e of
  Just (NegativeIndex i) -> expected == i
  Nothing -> False
negativeIndex _ _ = False

