{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  parseFilter,
  exprParser
) where

import Control.Monad (void)

import Data.Error.Trace (EitherTrace, ofEither)
import qualified Data.JSON as JSON
import Data.JsonStream (Streamset, getStreams, addStream)
import Data.Text (Text, pack)
import Language.Eval (Eval, eval)

import Parser.Core (consumeEverything, lexeme, parse)
import Parser.JSON (Parser, punctuation)
import Parser.Language (exprParser)

import Text.Megaparsec (between, chunk, option, sepBy, some, try)
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

parseFilter :: Text -> EitherTrace Filter
parseFilter = parse parser ""

parser :: Monad m => Parser m Filter
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
  
