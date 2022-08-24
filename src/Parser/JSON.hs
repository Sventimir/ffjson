{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Parser.JSON (
  Parser,
  parseJSON,
  lexeme,
  string,
  number,
  constants,
  punctuation,
  space,
  array,
  object,
  json
) where

import Prelude hiding (null)
import Control.Applicative ((<|>), many)
import Data.Error.Trace (EitherTrace)
import Data.Function (fix)
import Data.JSON (JSON(..))
import Data.Text (Text)
import qualified Data.Text as Text

import Parser.Core (Parser, ParseError, parse, lexeme, space, punctuation, consumeEverything)

import Text.Megaparsec (between, anySingleBut, chunk, sepBy, try)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer



parseJSON :: JSON j => String -> Text -> EitherTrace j
parseJSON = parse (consumeEverything $ fix json)

string :: Monad m => Parser m Text
string = lexeme $
         fmap Text.pack $ quoted . many $ anySingleBut '"'

number :: (JSON j, Monad m) => Parser m j
number = lexeme . fmap num $ Lexer.signed space (try float <|> decimal)
  where
  float = toRational <$> (Lexer.float :: Parser m Double)
  decimal = toRational <$> (Lexer.decimal :: Parser m Integer)

constants :: (JSON j, Monad m) => Parser m j
constants = lexeme $
            ("null" `readAs` null) <|>
            ("false" `readAs` bool False) <|>
            ("true" `readAs` bool True)
  where
  readAs repr val = chunk repr >> return val

arr :: (JSON j, Monad m) => Parser m j -> Parser m j
arr self = lexeme $ do
  elems <- between (punctuation '[') (punctuation ']')
    $ sepBy self (punctuation ',')
  return $ array elems

object :: forall j m. (JSON j, Monad m) => Parser m j -> Parser m j
object self = lexeme $ do
  elems <- between (punctuation '{') (punctuation '}')
    $ sepBy keyValuePair (punctuation ',')
  return $ obj elems
  where
  keyValuePair :: Parser m (Text, j)
  keyValuePair = do
    key <- string
    _ <- lexeme $ char ':'
    value <- self
    return (key, value)


json :: (JSON j, Monad m) => Parser m j -> Parser m j
json self = fmap str string <|> number <|> constants <|> arr self <|> object self

quoted :: Monad m => Parser m a -> Parser m a
quoted = between (char '"') (char '"')
