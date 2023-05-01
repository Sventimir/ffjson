module Parser.Core (
  Parser,
  ParseError(..),
  TokenParser,
  TokParseError(..),
  parse,
  space,
  lexeme,
  punctuation,
  consumeEverything,
  mapLeft,
  -- Token parsers
  runTokenParser,
  tokFail,
  currentToken,
  tokenP,
  token,
  eof,
  between,
  many1,
  match,
  backtrack,
  skip,
  select,
  withSep,
  optional,
  withDefault
) where

import Control.Applicative (Alternative(..), many)
import Control.Monad.Catch (Exception)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT, gets, modify)
import Control.Monad.Trans.Class (lift)

import Data.Error.Trace (EitherTrace, ExceptTraceT, ofEither, liftEither, liftTrace)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.List.Zipper (Zipper, fromList, safeCursor, left, right)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)

import Text.Megaparsec (ParsecT)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty)
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as Lexer


newtype ParseError = ParseError (ParseErrorBundle Text Void)

instance Exception ParseError where

instance Show ParseError where
  show = errorBundlePretty . coerce


type Parser m a = ParsecT Void Text m a

parse :: Parser Identity a -> String -> Text -> EitherTrace a
parse p fname = ofEither . mapLeft ParseError . Megaparsec.parse p fname

space :: Monad m => Parser m ()
space = Lexer.space space1 Megaparsec.empty Megaparsec.empty

lexeme :: Monad m => Parser m a -> Parser m a
lexeme  = Lexer.lexeme space

punctuation :: Monad m => Char -> Parser m Char
punctuation = lexeme . char


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right c) = Right c

consumeEverything :: Monad m => Parser m a -> Parser m a
consumeEverything p = do
  a <- p
  () <- Megaparsec.eof
  return a

type TokenParser t m a = StateT (Zipper t) (ExceptTraceT m) a

data TokParseError t = EndOfInput
                     | Empty
                     | UnexpectedToken t String
                     | InvalidIndex t
                     | Undefined Text

instance Show t => Show (TokParseError t) where
  show Empty = "Exhausted possible actions."
  show EndOfInput = "Unexpected end of input."
  show (UnexpectedToken t reason) =
    "Unexpected token: '" <> show t <> "' where " <> reason <> " was expected."
  show (InvalidIndex t) = "Invalid index: " <> show t <> "."
  show (Undefined n) = "Undefined symbol: " <> show n <> "."

instance (Show t, Typeable t) => Exception (TokParseError t) where

runTokenParser :: Monad m => TokenParser t m a -> [t] -> ExceptTraceT m a
runTokenParser p tokens = evalStateT p (fromList tokens)

tokFail :: (Show t, Typeable t, Monad m) => TokParseError t -> TokenParser t m a
tokFail e = StateT $ \_ -> liftEither $ Left e

currentToken :: (Show t, Typeable t, Monad m) => TokenParser t m t
currentToken = gets safeCursor >>= failOnEOF
  where
  failOnEOF Nothing = tokFail EndOfInput
  failOnEOF (Just tok) = return tok

tokenP :: (Show t, Typeable t, Monad m) => String -> (t -> Bool) -> TokenParser t m t
tokenP expected predicate = do
  tok <- currentToken
  if predicate tok
  then do
    skip
    return tok
  else
    tokFail $ UnexpectedToken tok expected

token :: (Eq t, Show t, Typeable t, Monad m) => t -> TokenParser t m ()
token t = tokenP ("'" <> show t <> "'") (== t) >> return ()


between :: (Eq t, Show t, Typeable t, Monad m) =>
           TokenParser t m () -> TokenParser t m a -> TokenParser t m () ->
           TokenParser t m a
between prefix main suffix = do
  prefix
  ret <- main
  suffix
  return ret

skip :: Monad m => TokenParser t m ()
skip = modify right

eof :: (Show t, Typeable t, Monad m) => TokenParser t m ()
eof = gets safeCursor >>= isEof
  where
  isEof Nothing = return ()
  isEof (Just t) = tokFail $ UnexpectedToken t "end of input"

backtrack :: Monad m => TokenParser t m ()
backtrack = modify left

match :: (Show t, Typeable t, Monad m) =>
         (t -> EitherTrace a) -> TokenParser t m a
match f = do
  t <- currentToken
  ret <- lift . liftTrace $ f t
  skip
  return ret

select :: (Show t, Typeable t, Monad m) =>
           (t -> TokenParser t m a) -> TokenParser t m a
select selector = do
  origInput <- get
  case safeCursor origInput of
    Nothing -> tokFail EndOfInput
    Just t -> skip >> selector t

withSep :: (Show t, Typeable t, Monad m) =>
         TokenParser t m () -> TokenParser t m a -> TokenParser t m [a]
withSep separator element = reverse <$> accum []
 where
  accum acc = do
    acc' <- (: acc) <$> element
    -- Select continuation based on whether or not another separator can be parsed.
    cont <- (separator >> return accum) <|> return return
    cont acc'

many1 :: (Alternative m, Monad m) => m a -> m [a]
many1 p = reverse <$> (do
                          e <- p
                          (e :) <$> many p)

optional :: Monad m => TokenParser t m a -> TokenParser t m (Maybe a)
optional p = (Just <$> p) <|> return Nothing

withDefault :: Monad m => a -> TokenParser t m a -> TokenParser t m a
withDefault def p = optional p >>= maybe (return def) return
