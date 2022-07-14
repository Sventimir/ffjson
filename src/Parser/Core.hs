module Parser.Core (
  Parser,
  ParseError,
  parse
) where

import Control.Monad.Catch (Exception)

import Data.Error.Trace (EitherTrace, ofEither)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Text (Text, unpack)

import Text.Megaparsec (ParsecT)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle(..), ShowErrorComponent(..), errorBundlePretty)


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


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right c) = Right c
