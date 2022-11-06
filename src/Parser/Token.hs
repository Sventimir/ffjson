module Parser.Token (
  Token(..),
  TokParser,
  tokenizer,
  tokString,
  tokNumber,
  tokName,
  tokSymbol
) where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Control.Monad.Catch (Exception)
import Control.Monad.Identity (Identity)

import Data.Coerce (coerce)
import Data.Error.Trace (EitherTrace, ofEither)
import Data.JSON.Repr (numAsDecimal)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)

import Parser.Core (Parser, lexeme, space, mapLeft)

import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as Lexer


data Token = Name Text -- symbol consisting of alphanumeric characters
           | Sym Text  -- symbol consisting of non-alphanumeric characters
           | Num Rational
           | Str Text
           deriving (Eq, Ord)

instance Show Token where
  show (Name n) = Text.unpack n
  show (Sym s) = Text.unpack s
  show (Num n) = Text.unpack $ numAsDecimal n
  show (Str s) = '"' : Text.unpack s ++ "\""

  
newtype TokParseError = TokParseError (ParseErrorBundle [Token] Void)

instance Exception TokParseError where

instance Show TokParseError where
  show = Megaparsec.errorBundlePretty . coerce
  
type TokParser m a = Megaparsec.ParsecT Void [Token] m a


parseTokens :: TokParser Identity a -> String -> [Token] -> EitherTrace a
parseTokens p fname = ofEither . mapLeft TokParseError . Megaparsec.parse p fname

tokenizer :: Monad m => Parser m [Token]
tokenizer = tokens []

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
number = lexeme $ Lexer.signed space (Megaparsec.try float <|> decimal)
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

tokString :: Monad m => TokParser m Text
tokString = Megaparsec.token match
          . Set.singleton $ Megaparsec.Label ('A' :| " string was expected.")
  where
  match (Str s) = Just s
  match _ = Nothing

tokNumber :: Monad m => TokParser m Rational
tokNumber = Megaparsec.token match
          . Set.singleton $ Megaparsec.Label ('A' :| " number was expected.")
  where
  match (Num n) = Just n
  match _ = Nothing

tokSymbol :: Monad m => Text -> TokParser m ()
tokSymbol = void . Megaparsec.single . Sym

tokName :: Monad m => Text -> TokParser m ()
tokName = void . Megaparsec.single . Name 

