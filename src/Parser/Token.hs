module Parser.Token (
  Getter(..),
  Token(..),
  tokenize
) where

import Control.Applicative (Alternative(..))

import Data.JSON.Repr (numAsDecimal)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)

import Parser.Core (Parser, lexeme)

import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as Lexer


-- A list-like structure gathering getter compositions.
data Getter = ArrayGetter Int (Maybe Getter)
            | ArraySlice (Maybe Int, Maybe Int, Maybe Int) (Maybe Getter)
            | ObjectGetter Text (Maybe Getter)
            deriving (Eq, Ord, Typeable)


showOpt :: Show a => Maybe a -> String
showOpt Nothing = ""
showOpt (Just x) = show x

instance Show Getter where
  show (ArrayGetter i g) = ".[" ++ show i ++ "]" ++ showOpt g
  show (ArraySlice (first, last, step) g) =
      let idxs = showOpt <$> [first, last, step] in
      ".[" ++ intercalate ":" idxs ++ "]" ++ showOpt g
    where
    showIdxs [] = ":"
    showIdxs idxs = intercalate ":" $ map show idxs
  show (ObjectGetter k g) = "." ++ Text.unpack k ++ "" ++ showOpt g
    

data Token = Name Text -- symbol consisting of alphanumeric characters
           | Sym Text  -- symbol consisting of non-alphanumeric characters
           | Num Rational
           | Str Text
           | Getter Getter
           deriving (Eq, Ord, Typeable)

instance Show Token where
  show (Name n) = Text.unpack n
  show (Sym s) = Text.unpack s
  show (Num n) = Text.unpack $ numAsDecimal n
  show (Str s) = '"' : Text.unpack s ++ "\""
  show (Getter g) = show g

tokenize :: Monad m => Parser m [Token]
tokenize = tokens []

tokens :: Monad m => [Token] -> Parser m [Token]
tokens acc = do
  maybeSymbol <- Megaparsec.optional (Getter <$> lexeme getter
                                  <|> Str <$> lexeme string
                                  <|> Num <$> lexeme number
                                  <|> Name <$> lexeme name 
                                  <|> Sym <$> lexeme symbol)
  case maybeSymbol of
    Nothing -> return $ reverse acc
    Just s -> tokens (s : acc)

getter :: Monad m => Parser m Getter
getter = do
    _ <- MegaparsecChar.char '.'
    parseArrGetter <|> parseObjGetter
  where
  index = Megaparsec.optional . lexeme
          $ Lexer.signed MegaparsecChar.space Lexer.decimal
  parseArrGetter = do
    _ <- MegaparsecChar.char '['
    idx <- index
    slice <- Megaparsec.optional $ do
      _ <- MegaparsecChar.char ':'
      l <- index
      _ <- Megaparsec.optional $ MegaparsecChar.char ':'
      s <- index
      return (l, s)
    _ <- MegaparsecChar.char ']'
    g <- Megaparsec.optional getter
    case (idx, slice) of
      (Nothing, Nothing) -> fail "missing index or slice"
      (Just i, Nothing) -> return $ ArrayGetter i g
      (first, Just (lst, step)) ->
        return $ ArraySlice (first, lst, step) g
  parseObjGetter = do
    k <- name <|> string
    g <- Megaparsec.optional getter
    return $ ObjectGetter k g
  
string :: Monad m => Parser m Text
string = Text.pack <$> (qmark >> Megaparsec.manyTill Lexer.charLiteral qmark)
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
