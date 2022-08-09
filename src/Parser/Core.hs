module Parser.Core (
  Parser,
  ParseError,
  parse,
  space,
  lexeme,
  punctuation,
  consumeEverything
) where

import Control.Monad.Catch (Exception)

import Data.Error.Trace (EitherTrace, ofEither)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Text (Text, unpack)

import Text.Megaparsec (ParsecT)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle(..), ShowErrorComponent(..), errorBundlePretty)
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as Lexer


newtype InternalParseError = UnexpectedToken Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent InternalParseError where
  errorComponentLen = length . showErrorComponent
  showErrorComponent (UnexpectedToken tok) =
    "Unexpected token: '" <> unpack tok <> "'."


newtype ParseError = ParseError (ParseErrorBundle Text InternalParseError)

instance Exception ParseError where

instance Show ParseError where
  show = errorBundlePretty . coerce


type Parser m a = ParsecT InternalParseError Text m a

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
