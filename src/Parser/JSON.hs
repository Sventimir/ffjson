{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Parser.JSON (
  Parser,
  ParseError(..),
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
import Data.Error.Trace (EitherTrace, ofEither)
import Data.Function (fix)
import Data.JSON (JSON(..), JsonStream(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (ParsecT, ParseErrorBundle, empty, between, anySingleBut,
                        chunk, sepBy, parse, try)
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (ShowErrorComponent(..))


data ParseError = UnexpectedToken Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseError where
  errorComponentLen = length . showErrorComponent
  showErrorComponent (UnexpectedToken tok) =
    "Unexpected token: '" <> Text.unpack tok <> "'."

type Parser m a = ParsecT ParseError Text m a

parseJSON :: JSON j => Text -> EitherTrace j
parseJSON = ofEither . parse (fix json) ""

space :: Monad m => Parser m ()
space = Lexer.space space1 empty empty

lexeme :: Monad m => Parser m a -> Parser m a
lexeme  = Lexer.lexeme space

string :: Monad m => Parser m Text
string = lexeme $
         fmap Text.pack $ quoted . many $ anySingleBut '"'

punctuation :: Monad m => Char -> Parser m Char
punctuation = lexeme . char

number :: (JSON j, Monad m) => Parser m j
number = lexeme . fmap num $ Lexer.signed space (try Lexer.float <|> Lexer.decimal)

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
    lexeme $ char ':'
    value <- self
    return (key, value)


json :: (JSON j, Monad m) => Parser m j -> Parser m j
json self = fmap str string <|> number <|> constants <|> arr self <|> object self

quoted :: Monad m => Parser m a -> Parser m a
quoted = between (char '"') (char '"')


someJSON :: JsonStream
someJSON = JsonStream $ obj []
