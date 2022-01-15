{-# LANGUAGE OverloadedStrings #-}
module Parser.JSON (
  ParseError(..),
  parseJSON
) where

import Prelude hiding (null)
import Control.Applicative ((<|>), many)
import Data.JSON (JSON(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (ParsecT, ParseErrorBundle, empty, between, anySingleBut,
                        chunk, sepBy, parse)
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

parseJSON :: JSON json => Text -> Either (ParseErrorBundle Text ParseError) json
parseJSON = parse json ""

space :: Monad m => Parser m ()
space = Lexer.space space1 empty empty

lexeme :: Monad m => Parser m a -> Parser m a
lexeme  = Lexer.lexeme space

string :: Monad m => Parser m Text
string = lexeme $
         fmap Text.pack $ quoted . many $ anySingleBut '"'

number :: (JSON j, Monad m) => Parser m j
number = lexeme $ fmap num Lexer.float

constants :: (JSON j, Monad m) => Parser m j
constants = lexeme $
            ("null" `readAs` null) <|>
            ("false" `readAs` bool False) <|>
            ("true" `readAs` bool True)
  where
  readAs repr val = chunk repr >> return val

arr :: (JSON j, Monad m) => Parser m j
arr = lexeme $ do
  elems <- between (char '[') (char ']') $ sepBy json (char ',')
  return $ array elems

object :: (JSON j, Monad m) => Parser m j
object = lexeme $ do
  elems <- between (char '{') (char '}') $ sepBy keyValuePair (char ',')
  return $ obj elems
  where
  keyValuePair :: (JSON j, Monad m) => Parser m (Text, j)
  keyValuePair = do
    key <- string
    lexeme $ char ':'
    value <- json
    return (key, value)


json :: (JSON j, Monad m) => Parser m j
json = fmap str string <|> number <|> constants <|> arr <|> object

quoted :: Monad m => Parser m a -> Parser m a
quoted = between (char '"') (char '"')
