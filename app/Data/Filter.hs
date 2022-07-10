{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  parse,
  exprParser
) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception(..))

import Data.Error.Trace (EitherTrace, eitherJustTrace, ofEither)
import Data.JSON (JSON)
import Data.JsonStream (Streamset, getStream, addStream)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Language.Eval (Eval, eval)
import Language.Functions (Functions)
import qualified Language.Functions as Functions
import Language.Syntax (Syntax)
import qualified Language.Syntax as Syntax

import Parser.JSON (Parser, punctuation)
import qualified Parser.JSON as JsonParser

import Text.Megaparsec (between, optional, some)
import Text.Megaparsec.Char (alphaNumChar)
import qualified Text.Megaparsec as Megaparsec


data Filter = Filter {
    inputKey, outputKey :: Text,
    filterExpr :: Eval
  }

newtype FilterError = UnknownStream Text
  deriving Show

instance Exception FilterError where
  

evaluate :: Filter -> Streamset -> EitherTrace Streamset
evaluate flt streams = do
  let k = inputKey flt
  input <- eitherJustTrace (UnknownStream k) (getStream k streams)
  output <- eval (filterExpr flt) input
  return $ addStream (outputKey flt) output streams

parse :: Text -> EitherTrace Filter
parse = ofEither . Megaparsec.parse parser ""

parser :: Monad m => Parser m Filter
parser = do
  inKey <- fromMaybe "0" <$> optional key
  expr <- exprParser
  outKey <- fromMaybe "0" <$> optional key
  return $ Filter inKey outKey expr

exprParser :: (Monad m, JSON j, Syntax j, Functions j) => Parser m j
exprParser = Syntax.parser (JsonParser.json exprParser <|> Functions.parser exprParser)

key :: Monad m => Parser m Text
key = between
  (punctuation '[')
  (punctuation ']')
  (pack <$> some alphaNumChar)
  
