module Parser.Core (
  Parser,
  ParseError(..),
  parse,
  space,
  lexeme,
  punctuation,
  consumeEverything,
  mapLeft
) where

import Control.Monad.Catch (Exception)

import Data.Error.Trace (EitherTrace, ofEither)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
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
