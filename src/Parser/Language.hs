{-# LANGUAGE OverloadedStrings #-}
module Parser.Language
  ( tokExpr
  ) where

import Control.Applicative (Alternative(..))

import Data.JSON (JSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (numerator, denominator)
import Data.Text (Text)

import Language.Syntax (Syntax)
import qualified Language.Syntax as Syntax
import Language.Functions (Functions)
import qualified Language.Functions as Functions

import Parser.Core (TokenParser, TokParseError(..), currentToken,
                    select, token, tokFail, optional)
import Parser.Token (Token(..))
import qualified Parser.JSON as JsonParser


tokExpr :: (Monad m, Syntax j, Functions j) => TokenParser Token m j
tokExpr = tokOperator funArg
  where
  funArg = tokGetter <|> tokFunction funArg <|> tokConditional tokExpr
       <|> JsonParser.tokJSON tokExpr <|> tokParentheses tokExpr

tokParentheses :: Monad m => TokenParser Token m j -> TokenParser Token m j
tokParentheses expr = do
    token $ Sym "("
    e <- expr
    token $ Sym ")"
    return e

tokFunction :: (Monad m, JSON j, Functions j) => TokenParser Token m j -> TokenParser Token m j
tokFunction arg = select fun
  where
  fun (Name n) = case Map.lookup n tokFunctions of
    Nothing -> tokFail $ Undefined n
    Just p -> p arg
  fun t = tokFail $ UnexpectedToken t "a function name"

tokOperator :: (Monad m, JSON j, Functions j) => TokenParser Token m j -> TokenParser Token m j
tokOperator subExpr = foldr parseOp subExpr tokOperators
  where
  parseOp (symbol, f) expr = do
    e <- expr
    es <- many $ do
      token $ Sym symbol
      expr
    return $ foldl f e es

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

tokFunctions :: (Monad m, JSON j, Functions j) => Map Text (TokenParser t m j -> TokenParser t m j)
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
                            , ("minus", fun2 Functions.minus)
                            , ("mult", fun2 Functions.mult)
                            , ("div", fun2 Functions.divide)
                            , ("union", fun2 Functions.union)
                            , ("concat", fun2 Functions.concat)
                            , ("and", fun2 Functions.and)
                            , ("or", fun2 Functions.or)
                            , ("compose", fun2 Functions.compose)
                            ]
  where
  fun f arg = f <$> arg
  fun2 f arg = f <$> arg <*> arg

tokOperators :: (JSON j, Functions j) => [(Text, (j -> j -> j))]
tokOperators = [ ("|", Functions.compose)
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
    
