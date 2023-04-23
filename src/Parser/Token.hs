module Parser.Token (
  Token(..),
  tokenize
) where

import Control.Applicative (Alternative(..))

import Data.JSON.Repr (numAsDecimal)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)

import Parser.Core (Parser, lexeme, space)

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as Lexer



data Token = Name Text -- symbol consisting of alphanumeric characters
           | Sym Text  -- symbol consisting of non-alphanumeric characters
           | Num Rational
           | Str Text
           deriving (Eq, Ord, Typeable)

instance Show Token where
  show (Name n) = Text.unpack n
  show (Sym s) = Text.unpack s
  show (Num n) = Text.unpack $ numAsDecimal n
  show (Str s) = '"' : Text.unpack s ++ "\""

tokenize :: Monad m => Parser m [Token]
tokenize = tokens []

tokens :: Monad m => [Token] -> Parser m [Token]
tokens acc = do
  maybeSymbol <- Megaparsec.optional (Str <$> lexeme string
                                  <|> Num <$> lexeme number
                                  <|> Name <$> lexeme name 
                                  <|> Sym <$> lexeme symbol)
  case maybeSymbol of
    Nothing -> return $ reverse acc
    Just s -> tokens (s : acc)
  
  
string :: Monad m => Parser m Text
string = lexeme $ Text.pack <$> (qmark >> Megaparsec.manyTill Lexer.charLiteral qmark)
  where
  qmark = MegaparsecChar.char '"'

number :: Monad m => Parser m Rational
number = lexeme (Megaparsec.try float <|> decimal)
  where
  float = toRational <$> (Lexer.float :: Parser m Double)
  decimal = toRational <$> (Lexer.decimal :: Parser m Integer)

name :: Monad m => Parser m Text
name = Text.pack <$> many1 MegaparsecChar.alphaNumChar

symbol :: Monad m => Parser m Text
symbol = Text.pack <$> sym
  where
  sym = many1 MegaparsecChar.symbolChar
    <|> (return <$> MegaparsecChar.punctuationChar)

many1 :: Monad m => Parser m a -> Parser m [a]
many1 p = do
  r <- Megaparsec.many p
  case r of
    [] -> fail "No occurrences"
    rs -> return rs
