{-# LANGUAGE OverloadedStrings #-}
module Data.Filter (
  Filter,
  evaluate,
  sourceCode,
  parseFilter
) where

import Control.Monad.Catch (Exception)
import Control.Monad.State (StateT(..))

import Data.Error.Trace (EitherTrace, ExceptTraceT, traceError, liftEither, liftTrace)
import qualified Data.JSON as JSON
import qualified Data.JSON.AST as AST
import Data.JSON.Repr (reprS, defaultReprConfig)
import Data.JsonStream (Streamset, getStreams, addStream)
import Data.Ratio (denominator, numerator)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Language.Eval (Eval, eval)

import Parser.Core (TokenParser, parse, select, runTokenParser, withSep,
                    withDefault, token)
import Parser.Language (tokExpr)
import Parser.Token (Token(..), tokenize)



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

parseFilter :: Monad m => Text -> ExceptTraceT m Filter
parseFilter src = do
  tokens <- liftTrace $ parse tokenize "<command line>" src
  ($ src) <$> runTokenParser parser tokens

parser :: Monad m => TokenParser Token m (Text -> Filter)
parser = do
  inKeys <- withDefault [] $ do
    ks <- withSep (token $ Sym "&") key
    token (Sym ">>")
    return ks
  expr <- tokExpr
  outKey <- withDefault "0" $ do
    token (Sym ">>")
    key
  return $ Filter inKeys outKey expr

key :: Monad m => TokenParser Token m Text
key = select nameOrNumber
  where
  nameOrNumber (Name n) = return n
  nameOrNumber (Num n)
    | denominator n == 1 = return . pack . show $ numerator n
    | otherwise = tokFail $ SourceCode ("Key must be an integer, not "
                                        <> (pack $ show n) <> ".")
  nameOrNumber t = tokFail $ SourceCode ("Invalid key: '"
                                         <> (pack $ show t) <> "'.")
  
tracedEval :: Filter -> AST.JsonAst -> EitherTrace AST.JsonAst
tracedEval flt input =
  traceError src $ traceError json $ eval (filterExpr flt) input
  where
  src = SourceCode $ sourceCode flt
  json = JsonCode $ reprS (AST.toJSON input) defaultReprConfig id

tokFail :: (Show t, Typeable t, Monad m) => FilterErrorDetails -> TokenParser t m a
tokFail e = StateT $ \_ -> liftEither $ Left e
