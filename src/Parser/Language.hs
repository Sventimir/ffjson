{-# LANGUAGE OverloadedStrings #-}
module Parser.Language (
  exprParser
) where

import Control.Applicative (Alternative(..))

import Data.JSON (JSON(..))
import Data.Maybe (fromMaybe)
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
exprParser = operators (
  Megaparsec.try (conditional exprParser)
  <|> functions immediateExpr
  <|> immediateExpr)
  where
  immediateExpr = constant "id" Functions.identity
                <|> JsonParser.json exprParser
                <|> getter
                <|> parentheses exprParser


getter :: (Monad m, Functions j, Syntax j) => Parser m j
getter = lexeme $ do
  -- the parser below is guaranteed to return a none-empty list.
  e : es <- some (
    Megaparsec.try getArray <|>
    Megaparsec.try quotedGetObject <|>
    getObject)
  return $ foldl Functions.compose e es


getObject :: (Monad m, Syntax j) => Parser m j
getObject = do
  _ <- MegaparsecChar.char '.'
  key <- some MegaparsecChar.alphaNumChar
  return . Syntax.get $ Text.pack key

quotedGetObject ::(Monad m, Syntax j) => Parser m j
quotedGetObject = do
  _ <- MegaparsecChar.char '.'
  key <- Megaparsec.between (MegaparsecChar.char '"') (punctuation '"')
         . many $ Megaparsec.anySingleBut '"'
  return . Syntax.get $ Text.pack key

getArray :: (Monad m, Syntax j) => Parser m j
getArray = do
  _ <- MegaparsecChar.string ".["
  idx <- Megaparsec.optional index
  ret <- optionally (idxOrSlice idx) $ do
    _ <- char ':'
    optionally (arrSlice idx Nothing Nothing) $ do
      to <- Megaparsec.optional index
      optionally (arrSlice idx to Nothing) $ do
        _ <- char ':'
        arrSlice idx to <$> Megaparsec.optional index
  _ <- char ']'
  return ret
  where
  idxOrSlice Nothing = arrSlice Nothing Nothing Nothing
  idxOrSlice (Just i) = Syntax.index i
  optionally dft p = fromMaybe dft <$> Megaparsec.optional p
  char = MegaparsecChar.char
  index = Lexer.signed space Lexer.decimal
  arrSlice f t s = Syntax.slice $ Syntax.ArraySlice f t s

  
conditional :: (Monad m, Syntax j) => Parser m j -> Parser m j
conditional subexpr = do
  _ <- keyword "if"
  cond <- subexpr
  _ <- keyword "then"
  ifSo <- subexpr
  _ <- keyword "else"
  ifNot <- subexpr
  return $ Syntax.ifThenElse cond ifSo ifNot
  where
  keyword = lexeme . MegaparsecChar.string

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

operator :: (Monad m, Functions j) => Parser m j -> Text -> (j -> j -> j) -> Parser m j
operator subexpr symbol f = do
  leftArg <- subexpr
  Megaparsec.option leftArg $ do
    _ <- lexeme $ Megaparsec.chunk symbol
    f leftArg <$> operator subexpr symbol f

functions :: (Monad m, Functions j) => Parser m j -> Parser m j
functions subexpr = foldl1 (<|>) $ map Megaparsec.try [
    function subexpr "keys" Functions.keys,
    function subexpr "size" Functions.size,
    function subexpr "map" Functions.jmap,
    function subexpr "filter" Functions.jfilter,
    function subexpr "sum" Functions.jsum,
    function subexpr "product" Functions.jproduct,
    function subexpr "all" Functions.jall,
    function subexpr "any" Functions.jany,
    function subexpr "not" Functions.not,
    function subexpr "neg" Functions.neg,
    function subexpr "recip" Functions.recipr,
    function subexpr "isNull" Functions.isNull,
    function subexpr "try" Functions.try,
    function2 subexpr "plus" Functions.plus,
    function2 subexpr "mult" Functions.mult,
    function2 subexpr "union" Functions.union,
    function2 subexpr "concat" Functions.concat,
    function2 subexpr "and" Functions.and,
    function2 subexpr "or" Functions.or,
    function2 subexpr "compose" Functions.compose
  ]

operators :: (Monad m, Functions j) => Parser m j -> Parser m j
operators subexpr = foldr combine subexpr [
    ("|", Functions.compose),
    ("?", Functions.optMap),
    ("&&", Functions.and),
    ("||", Functions.or),
    ("=", Functions.equal),
    ("<", Functions.lt),
    ("<=", Functions.lte),
    (">", Functions.gt),
    (">=", Functions.gte),
    ("+", Functions.plus),
    ("-", Functions.minus),
    ("*", Functions.mult),
    ("/", Functions.divide),
    ("<>", Functions.concat)
  ]
  where
  combine (symbol, f) acc = Megaparsec.try (operator acc symbol f)

parentheses :: (Monad m, Syntax j) => Parser m j -> Parser m j
parentheses = Megaparsec.between (punctuation '(') (punctuation ')')
