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
import Parser.Token (Getter(..), Token(..))
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
tokGetter = select getter
  where
  getter (Getter g) = return $ mkGetter g
  getter t = tokFail $ UnexpectedToken t "a getter"
  mkGetter :: (Syntax j, Functions j) => Getter -> j
  mkGetter (ArrayGetter i Nothing) = Syntax.index i
  mkGetter (ArrayGetter i (Just g)) = Syntax.index i `Functions.compose` mkGetter g
  mkGetter (ArraySlice (f, l, s) Nothing) =
    Syntax.slice $ Syntax.ArraySlice f l s
  mkGetter (ArraySlice (f, l, s) (Just g)) =
    (Syntax.slice $ Syntax.ArraySlice f l s) `Functions.compose` mkGetter g
  mkGetter (ObjectGetter k Nothing) = Syntax.get k
  mkGetter (ObjectGetter k (Just g)) = Syntax.get k `Functions.compose` mkGetter g

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
    
