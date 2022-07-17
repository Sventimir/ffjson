{-# LANGUAGE OverloadedStrings #-}
module Parser.Language (
  exprParser
) where

import Control.Applicative (Alternative(..))

import Data.JSON (JSON(..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Syntax (Syntax)
import qualified Language.Syntax as Syntax
import Language.Functions (Functions)
import qualified Language.Functions as Functions

import Parser.Core (Parser, lexeme, punctuation, space)
import qualified Parser.JSON as JsonParser

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as Lexer


exprParser :: (Monad m, JSON j, Syntax j, Functions j) => Parser m j
exprParser = do
  e <- simpleExprParser exprParser
  withOperators e $ simpleExprParser exprParser

withOperators :: (Monad m, Functions j) => j -> Parser m j -> Parser m j
withOperators acc expr =
  do
    f <- Megaparsec.try operator
    e <- expr
    withOperators (f acc e) expr
  <|> return acc
  
  

simpleExprParser :: (Monad m, JSON j, Syntax j, Functions j) => Parser m j -> Parser m j
simpleExprParser subexpr =
  Megaparsec.try (JsonParser.json subexpr)
  <|> Megaparsec.try getter
  <|> Megaparsec.try (functions subexpr)
  <|> parentheses subexpr


getter :: (Monad m, Functions j, Syntax j) => Parser m j
getter = do
  -- the parser below is guaranteed to return a none-empty list.
  e : es <- some (
    Megaparsec.try getArray <|>
    Megaparsec.try quotedGetObject <|>
    getObject)
  return $ foldl Functions.compose e es


getObject :: (Monad m, Syntax j) => Parser m j
getObject = lexeme $ do
  _ <- MegaparsecChar.char '.'
  key <- some MegaparsecChar.alphaNumChar
  return . Syntax.get $ Text.pack key

quotedGetObject ::(Monad m, Syntax j) => Parser m j
quotedGetObject = lexeme $ do
  _ <- MegaparsecChar.char '.'
  key <- Megaparsec.between (MegaparsecChar.char '"') (punctuation '"')
         . many $ Megaparsec.anySingleBut '"'
  return . Syntax.get $ Text.pack key
  
getArray :: (Monad m, Syntax j) => Parser m j
getArray = do
  _ <- lexeme $ MegaparsecChar.string ".["
  i <- lexeme $ Lexer.signed space Lexer.decimal
  _ <- lexeme $ MegaparsecChar.char ']'
  return $ Syntax.index i

constant :: (Monad m, Functions j) => Text -> j -> Parser m j
constant name c = lexeme (Megaparsec.chunk name) >> return c

function :: (Monad m, Functions j) => Parser m j -> Text -> (j -> j) -> Parser m j
function subexpr name f = do
  _ <- lexeme $ Megaparsec.chunk name
  f <$> subexpr

function2 :: (Monad m, Functions j) => Parser m j -> Text -> (j -> j -> j) -> Parser m j
function2 subexpr name f = do
  _ <- lexeme $ Megaparsec.chunk name
  arg <- subexpr
  f arg <$> subexpr

functions :: (Monad m, Functions j) => Parser m j -> Parser m j
functions subexpr = foldl1 (<|>) $ map Megaparsec.try [
    constant "id" Functions.identity,
    constant "keys" Functions.keys,
    function subexpr "map" Functions.jmap,
    function2 subexpr "plus" Functions.plus,
    function2 subexpr "compose" Functions.compose
  ]

operator :: (Monad m, Functions j) => Parser m (j -> j -> j)
operator = do
  op <- lexeme $ Megaparsec.some (MegaparsecChar.symbolChar <|> MegaparsecChar.punctuationChar)
  case Map.lookup op operators of
    Nothing -> fail "Unknown operator"
    Just f -> return f

operators :: Functions j => Map.Map String (j -> j -> j)
operators = Map.fromList [
    ("+", Functions.plus),
    ("|", Functions.compose)
  ]

parentheses :: (Monad m, Syntax j) => Parser m j -> Parser m j
parentheses = Megaparsec.between (punctuation '(') (punctuation ')')
