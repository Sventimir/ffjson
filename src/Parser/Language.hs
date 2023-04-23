{-# LANGUAGE OverloadedStrings #-}
module Parser.Language
  ( exprParser
  , tokExpr
  ) where

import Control.Applicative (Alternative(..))

import Data.JSON (JSON(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (numerator, denominator)
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Syntax (Syntax)
import qualified Language.Syntax as Syntax
import Language.Functions (Functions)
import qualified Language.Functions as Functions

import Parser.Core (Parser, TokenParser, lexeme, punctuation, space,
                    TokParseError(..), currentToken, select, token, tokFail, optional)
import Parser.Token (Token(..))
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
  -- the parser below is guaranteed to return a non-empty list.
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
    function subexpr "flatten" Functions.jflatten,
    function subexpr "unique" Functions.unique,
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

------------------------------------------------------
-- Token parser
------------------------------------------------------
tokExpr :: (Monad m, Syntax j, Functions j) => TokenParser Token m j
tokExpr = tokOperator (   tokGetter
                      <|> tokConditional tokExpr
                      <|> tokFunction funArg
                      <|> JsonParser.tokJSON tokExpr
                      <|> tokParentheses tokExpr
                      )
  where
  funArg = tokGetter <|> tokConditional tokExpr <|> tokFunction funArg
       <|> JsonParser.tokJSON tokExpr <|> tokParentheses tokExpr

tokParentheses :: Monad m => TokenParser Token m j -> TokenParser Token m j
tokParentheses expr = do
    token $ Sym "("
    e <- expr
    token $ Sym ")"
    return e

tokFunction :: (Monad m, Functions j) => TokenParser Token m j -> TokenParser Token m j
tokFunction arg = select fun
  where
  fun (Name n) = case Map.lookup n tokFunctions of
    Nothing -> tokFail $ Undefined n
    Just p -> p arg
  fun t = tokFail $ UnexpectedToken t "a function name"

tokOperator :: (Monad m, Functions j) => TokenParser Token m j -> TokenParser Token m j
tokOperator expr = do
  e <- expr
  cont <- many $ do
    op <- select operator
    (,) op <$> expr
  case cont of
    [] -> return e
    ops -> return $ foldl (\l (op, r) -> op l r) e ops
  where
  operator (Sym s) = case Map.lookup s tokOperators of
                       Just op -> return op
                       Nothing -> tokFail $ Undefined s
  operator t = tokFail $ UnexpectedToken t "an operator"

tokConditional :: (Monad m, Syntax j) => TokenParser Token m j -> TokenParser Token m j
tokConditional expr = do
  token (Name "if")
  cond <- expr
  token (Name "then")
  ifSo <- expr
  token (Name "else")
  Syntax.ifThenElse cond ifSo <$> expr

tokGetter :: (Monad m, Syntax j, Functions j) => TokenParser Token m j
tokGetter = do
  expr <- (many $ do
            token (Sym ".")
            select variant)
  case expr of
    [] -> tokFail Empty
    e : es -> return $ foldl Functions.compose e es
  where
  variant (Str prop) = return $ Syntax.get prop
  variant (Name prop) = return $ Syntax.get prop
  variant (Sym "[") = indexOrSlice
  variant t = tokFail $ UnexpectedToken t "a property, index or slice"

indexOrSlice :: (Monad m, Syntax j) => TokenParser Token m j
indexOrSlice = do
  idx1 <- optional index
  c <- optional colon
  case (idx1, c) of
    (Nothing, Nothing) -> currentToken >>= (tokFail . InvalidIndex)
    (Just i, Nothing) -> do
      endSlice
      return $ Syntax.index i
    (_, Just ()) -> do
      idx2 <- optional index
      idx3 <- optional $ do
        colon
        index
      endSlice
      return . Syntax.slice $ Syntax.ArraySlice idx1 idx2 idx3

  where
  colon = token (Sym ":")
  endSlice = token (Sym "]")

index :: Monad m => TokenParser Token m Int
index = select number
  where
  number (Num n) = indexToken n
  number (Sym "-") = negate <$> index
  number t = tokFail $ UnexpectedToken t "an integer"

indexToken :: Monad m => Rational -> TokenParser Token m Int
indexToken n
  | denominator n == 1 = return . fromInteger $ numerator n
  | otherwise = tokFail . InvalidIndex $ Num n

tokFunctions :: (Monad m, Functions j) => Map Text (TokenParser t m j -> TokenParser t m j)
tokFunctions = Map.fromList [ ("id", const $ return Functions.identity)
                            , ("keys", fun Functions.keys)
                            , ("compose", fun2 Functions.compose)
                            , ("concat", fun2 Functions.concat)
                            , ("size", fun Functions.size)
                            , ("map", fun Functions.jmap)
                            , ("filter", fun Functions.jfilter)
                            , ("flatten", fun Functions.jflatten)
                            , ("unique", fun Functions.unique)
                            , ("sum", fun Functions.jsum)
                            , ("product", fun Functions.jproduct)
                            , ("all", fun Functions.jall)
                            , ("any", fun Functions.jany)
                            , ("not", fun Functions.not)
                            , ("neg", fun Functions.neg)
                            , ("recip", fun Functions.recipr)
                            , ("isNull", fun Functions.isNull)
                            , ("try", fun Functions.try)
                            , ("plus", fun2 Functions.plus)
                            , ("mult", fun2 Functions.mult)
                            , ("union", fun2 Functions.union)
                            , ("concat", fun2 Functions.concat)
                            , ("and", fun2 Functions.and)
                            , ("or", fun2 Functions.or)
                            , ("compose", fun2 Functions.compose)
                            ]
  where
  fun f arg = f <$> arg
  fun2 f arg = f <$> arg <*> arg

tokOperators :: Functions j => Map Text (j -> j -> j)
tokOperators = Map.fromList [ ("|", Functions.compose)
                            , ("?", Functions.optMap)
                            , ("&&", Functions.and)
                            , ("||", Functions.or)
                            , ("=", Functions.equal)
                            , ("<", Functions.lt)
                            , ("<=", Functions.lte)
                            , (">", Functions.gt)
                            , (">=", Functions.gte)
                            , ("+", Functions.plus)
                            , ("-", Functions.minus)
                            , ("*", Functions.mult)
                            , ("/", Functions.divide)
                            , ("<>", Functions.concat)
                            ]
    
