{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.JSON.Repr (
  Repr(..),
  toText,
  reprS
) where

import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Control.Monad.Trans.Cont (Cont, runCont)

import Data.Coerce (coerce)
import Data.JSON (JSON(..))
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text


toText :: Show a => a -> Text
toText = Text.pack . show


newtype Indentation = Indentation (Int, Int)

modifyReturning :: Monad m => (Int -> Int -> Int) -> StateT Indentation m Text
modifyReturning f = do
  Indentation (cur, step) <- get
  let cur' = f cur step
  put $ Indentation (cur', step)
  return $ if step > 0 then '\n' `Text.cons` Text.replicate cur' " " else ""

indent :: Monad m => StateT Indentation m Text
indent = modifyReturning (+)

unindent :: Monad m => StateT Indentation m Text
unindent = modifyReturning (-)

indentation :: Indentation -> Int
indentation (Indentation (current, _)) = current


newtype Repr r = Repr (StateT Indentation (Cont r) Text)

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

withCommas :: Monad m => [StateT Indentation m Text] -> StateT Indentation m Text
withCommas = fmap Text.concat . sequence . intersperse comma
  where
  comma = do
    i <- gets indentation
    if i > 0 then return (",\n" <> Text.replicate i " ") else return ","

enclose :: Char -> StateT Indentation (Cont r) Text -> Char -> StateT Indentation (Cont r) Text
enclose hd body tl =
    (indent >>= \i -> return $ hd `Text.cons` i) <>
    body <>
    (unindent >>= \i -> return $ i `Text.snoc`tl)

instance JSON (Repr r) where
  str s = Repr $ "\"" <> return s <> "\""
  num n = Repr $ return (toText n)
  bool True = Repr "true"
  bool False = Repr "false"
  null = Repr "null"
  array js = Repr $ enclose '[' (withCommas $ coerce js) ']'
  obj kvs = Repr $ enclose '{' (withCommas . coerce $ map kvPair kvs) '}'

reprS :: Repr r -> Int -> (Text -> r) -> r
reprS (Repr json) indentationStep =
  runCont (evalStateT json $ Indentation (0, indentationStep))

instance Show (Repr String) where
  show r = reprS r 0 Text.unpack

instance Show (Repr Text) where
  show r = Text.unpack $ reprS r 0 id
