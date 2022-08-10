{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.JSON.Repr (
  Repr(..),
  ReprConfig(..),
  toText,
  reprS,
  defaultReprConfig
) where

import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans.Cont (Cont, runCont)

import Data.Coerce (coerce)
import Data.JSON (JSON(..))
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text

toText :: Show a => a -> Text
toText = Text.pack . show

data ReprConfig = ReprConfig {
    indentationStep :: Int,
    printRationals :: Bool -- if True, fractions are printed instead of decimals.
  }
  deriving Show

defaultReprConfig :: ReprConfig
defaultReprConfig = ReprConfig {
    indentationStep = 0,
    printRationals = False
  }

data State = State {
    indentation :: Int,
    indentStep :: Int,
    printNum :: Rational -> Text
  }

modifyReturning :: Monad m => (Int -> Int -> Int) -> StateT State m Text
modifyReturning f = do
  s <- get
  let ind = f (indentation s) (indentStep s)
  put $ s { indentation = ind }
  return $ if indentStep s > 0 then '\n' `Text.cons` Text.replicate ind " " else ""

indent :: Monad m => StateT State m Text
indent = modifyReturning (+)

unindent :: Monad m => StateT State m Text
unindent = modifyReturning (-)


newtype Repr r = Repr (StateT State (Cont r) Text)

instance (Monad m, Semigroup a) => Semigroup (m a) where
  ma <> mb = do
    a <- ma
    b <- mb
    return (a <> b)

instance IsString a => IsString (StateT s (Cont r) a) where
  fromString = return . fromString

kvPair :: (Text, Repr r) -> Repr r
kvPair (key, Repr reprVal) = Repr $ do
  val <- reprVal
  "\"" <> return key <> colon <> return val
  where
  colon = do
    i <- gets indentation
    if i > 0 then "\": " else "\":"

withCommas :: Monad m => [StateT State m Text] -> StateT State m Text
withCommas = fmap Text.concat . sequence . intersperse comma
  where
  comma = do
    i <- gets indentation
    if i > 0 then return (",\n" <> Text.replicate i " ") else return ","

enclose :: Char -> StateT State (Cont r) Text -> Char -> StateT State (Cont r) Text
enclose hd body tl =
    (indent >>= \i -> return $ hd `Text.cons` i) <>
    body <>
    (unindent >>= \i -> return $ i `Text.snoc`tl)

instance JSON (Repr r) where
  str s = Repr $ "\"" <> return s <> "\""
  num n = Repr $ gets (($ n) . printNum)
  bool True = Repr "true"
  bool False = Repr "false"
  null = Repr "null"
  array js = Repr $ enclose '[' (withCommas $ coerce js) ']'
  obj kvs = Repr $ enclose '{' (withCommas . coerce $ map kvPair kvs) '}'

reprS :: Repr r -> ReprConfig -> (Text -> r) -> r
reprS (Repr json) cfg =
  runCont (evalStateT json $ State {
              indentation = 0,
              indentStep = indentationStep cfg,
              printNum = if printRationals cfg then numAsRatio else numAsDecimal})
  where
  numAsDecimal n
    | denominator n == 1 = toText $ numerator n
    | otherwise = toText (fromRational n :: Double)
  numAsRatio n
    | denominator n == 1 = toText $ numerator n
    | otherwise = toText (numerator n) <> " / " <> toText (denominator n)

instance Show (Repr String) where
  show r = reprS r defaultReprConfig Text.unpack

instance Show (Repr Text) where
  show r = Text.unpack $ reprS r defaultReprConfig id
