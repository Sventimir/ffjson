{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  sourceCode,
  parseFilter,
  exprParser
) where

import Control.Monad (void)
import Control.Monad.Catch (Exception)

import Data.Error.Trace (EitherTrace, traceError)
import qualified Data.JSON as JSON
import qualified Data.JSON.AST as AST
import Data.JSON.Repr (reprS, defaultReprConfig)
import Data.JsonStream (Streamset, getStreams, addStream)
import Data.Text (Text, pack, unpack)
import Language.Eval (Eval, eval)

import Parser.Core (consumeEverything, lexeme, parse)
import Parser.JSON (Parser, punctuation)
import Parser.Language (exprParser)

import Text.Megaparsec (chunk, option, sepBy, some, try)
import Text.Megaparsec.Char (alphaNumChar)


data Filter = Filter {
    inputKey :: [Text],
    outputKey :: Text,
    filterExpr :: Eval,
    sourceCode :: Text
  }
  deriving Show

data FilterErrorDetails = SourceCode Text | JsonCode Text

instance Show FilterErrorDetails where
  show (SourceCode src) = "Source code: '" ++ unpack src ++ "'"
  show (JsonCode src) = "JSON: '" ++ unpack src ++ "'"

instance Exception FilterErrorDetails where

evaluate :: Filter -> Streamset -> EitherTrace Streamset
evaluate flt streams = do
  let keys = case inputKey flt of
        [] -> ["0"]
        ks -> ks
  ins <- getStreams keys streams
  let input = case ins of
        [] -> JSON.null
        [(_, i)] -> i
        _ -> JSON.obj ins
  output <- tracedEval flt input
  return $ addStream (outputKey flt) output streams

parseFilter :: Text -> EitherTrace Filter
parseFilter src = ($ src) <$> parse parser "<command line>" src

parser :: Monad m => Parser m (Text -> Filter)
parser = consumeEverything $ do
  inKey <- option [] . try $ do
    k <- sepBy key $ punctuation '&'
    () <- void . lexeme $ chunk ">>"
    return k
  expr <- exprParser
  outKey <- option "0" $ do
    () <- void . lexeme $ chunk ">>"
    key
  return $ Filter inKey outKey expr 

key :: Monad m => Parser m Text
key = pack <$> lexeme (some alphaNumChar)
  
tracedEval :: Filter -> AST.JsonAst -> EitherTrace AST.JsonAst
tracedEval flt input =
  traceError src $ traceError json $ eval (filterExpr flt) input
  where
  src = SourceCode $ sourceCode flt
  json = JsonCode $ reprS (AST.toJSON input) defaultReprConfig id
