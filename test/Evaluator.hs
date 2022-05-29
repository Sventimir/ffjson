{-# LANGUAGE OverloadedStrings #-}
module Evaluator ( evalTests ) where

import Prelude hiding (null)
import Test.Hspec
import Test.Hspec

import Control.Applicative ((<|>))
import Control.Monad (foldM)

import Data.Error.Trace (EitherTrace, ofEither, runEitherTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..))
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
      (".a" `applyTo` "{\"a\": 1}") `shouldBe` (num 1)

applyTo :: Text -> Text -> EitherTrace JsonAst
applyTo exprTxt jsonTxt = do
  json <- JsonParser.parseJSON jsonTxt
  expr <- ofEither $ Megaparsec.parse exprParser "" exprTxt
  eval expr json

exprParser :: (Monad m, JSON j, Syntax j, Functions j) => JsonParser.Parser m j
exprParser = JsonParser.json exprParser
             <|> Syntax.parser exprParser
             <|> Functions.parser


instance Eq a => Eq (EitherTrace a) where
  a == b = case (runEitherTrace a, runEitherTrace b) of
    (Right a, Right b) -> a == b
    (Left _, Left _) -> True
    _ -> False

instance JSON a => JSON (EitherTrace a) where
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
