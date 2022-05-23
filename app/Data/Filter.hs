{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  parse
) where

import Control.Monad.Catch (Exception(..), MonadThrow(..))

import Data.Error.Trace (EitherTrace, eitherJustTrace, ofEither)
import Data.JSON (JSON, JsonStream(..))
import Data.JSON.AST (JsonAst)
import Data.JsonStream (Streamset, getStream, addStream)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Language.Eval (Eval, eval)
import Language.Core (Composable(..))
import qualified Language.Syntax as Syntax

import Parser.JSON (Parser, lexeme, punctuation)

import Text.Megaparsec (between, many, optional, some)
import Text.Megaparsec.Char (char, alphaNumChar)
import qualified Text.Megaparsec as Megaparsec


data Filter = Filter {
    inputKey, outputKey :: Text,
    filterExpr :: Eval
  }

data FilterError = UnknownStream Text
  deriving Show

instance Exception FilterError where
  

evaluate :: Filter -> Streamset -> EitherTrace Streamset
evaluate flt streams = do
  let key = inputKey flt
  input <- eitherJustTrace (UnknownStream key) (getStream key streams)
  output <- eval (filterExpr flt) input
  return $ addStream (outputKey flt) output streams

parse :: Text -> EitherTrace Filter
parse = ofEither . Megaparsec.parse parser ""

parser :: Monad m => Parser m Filter
parser = do
  inputKey <- fmap (fromMaybe "0") $ optional key
  expr <- Syntax.parser
  outputKey <- fmap (fromMaybe "0") $ optional key
  return $ Filter inputKey outputKey expr

key :: Monad m => Parser m Text
key = between
  (punctuation '[')
  (punctuation ']')
  (fmap pack $ some alphaNumChar)
  
