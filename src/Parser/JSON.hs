{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Parser.JSON (
  Parser,
  punctuation,
  space,
  array,
  tokJSON
) where

import Prelude hiding (null)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(..))
import Data.JSON (JSON(..))

import Parser.Core (Parser, TokenParser, TokParseError(..), space,
                    punctuation, tokFail, select, token, match,
                    backtrack)
import Parser.Token (Token(..))


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
    
