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
  json,
  tokJSON
) where

import Prelude hiding (null)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(..))
import Data.Error.Trace (EitherTrace)
import Data.Function (fix)
import Data.JSON (JSON(..))
import Data.Text (Text)
import qualified Data.Text as Text

import Parser.Core (Parser, TokenParser, TokParseError(..), parse, lexeme,
                    space, punctuation, consumeEverything, tokFail, select,
                    token, match, backtrack)
import Parser.Token (Token(..))

import Text.Megaparsec (between, chunk, manyTill, sepBy, chunk, try)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer


parseJSON :: JSON j => String -> Text -> EitherTrace j
parseJSON = parse (consumeEverything $ fix json)

string :: Monad m => Parser m Text
string = lexeme $ Text.pack <$> (qmark >> manyTill Lexer.charLiteral qmark)
  where
  qmark = char '"'

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

tokJSON :: (JSON j, Monad m) => TokenParser Token m j -> TokenParser Token m j
tokJSON subexpr = select variant
  where
  unexpectedToken t = tokFail $ UnexpectedToken t "a string, number, constant, array or an object"
  variant (Name "null") = return null
  variant (Name "true") = return $ bool True
  variant (Name "false") = return $ bool False
  variant (Num n) = return $ num n
  variant (Str s) = return $ str s
  variant (Sym "-") = (num . negate <$> select number) <|> (backtrack >> unexpectedToken (Sym "-"))
  variant (Sym "[") = array <$> manyEnclosed (Sym "]") (Sym ",") subexpr
  variant (Sym "{") = obj <$> manyEnclosed (Sym "}") (Sym ",") kvPair
  variant tok = unexpectedToken tok
  kvPair = do
    k <- match string
    void $ token (Sym ":")
    v <- subexpr
    return (k, v)
  string (Str s) = return s
  string t = throwM $ UnexpectedToken t "a string"
  number (Num n) = return n
  number t = throwM $ UnexpectedToken t "a number"

manyEnclosed :: Monad m => Token -> Token -> TokenParser Token m j -> TokenParser Token m [j]
manyEnclosed endSymbol sep p = (token endSymbol >> return []) <|> accum []
  where
  accum acc = do
    el <- p
    select $ dispatch (el : acc)
  dispatch acc t
    | t == endSymbol = return $ reverse acc
    | t == sep = accum acc
    | otherwise = tokFail $ UnexpectedToken t "a comma or end of sequence"
    
