{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  parse,
  exprParser
) where

import Data.Error.Trace (EitherTrace, ofEither)
import qualified Data.JSON as JSON
import Data.JsonStream (Streamset, getStreams, addStream)
import Data.Text (Text, pack)
import Language.Eval (Eval, eval)

import Parser.Core (consumeEverything)
import Parser.JSON (Parser, punctuation)
import Parser.Language (exprParser)

import Text.Megaparsec (between, option, many, some)
import Text.Megaparsec.Char (alphaNumChar)
import qualified Text.Megaparsec as Megaparsec


data Filter = Filter {
    inputKey :: [Text],
    outputKey :: Text,
    filterExpr :: Eval
  }
  deriving Show

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
  output <- eval (filterExpr flt) input
  return $ addStream (outputKey flt) output streams

parse :: Text -> EitherTrace Filter
parse = ofEither . Megaparsec.parse parser ""

parser :: Monad m => Parser m Filter
parser = consumeEverything $ do
  inKey <- many key
  expr <- exprParser
  outKey <- option "0" key
  return $ Filter inKey outKey expr

key :: Monad m => Parser m Text
key = between
  (punctuation '[')
  (punctuation ']')
  (pack <$> some alphaNumChar)
  
