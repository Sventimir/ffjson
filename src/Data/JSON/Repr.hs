{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.JSON.Repr (
  reprS
) where

import Control.Monad.Trans.Cont (Cont, runCont)

import Data.Coerce (coerce)
import Data.JSON (JSON(..))
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text


toText :: Show a => a -> Text
toText = Text.pack . show


newtype Repr r = Repr (Cont r Text)

instance (Monad m, Semigroup a) => Semigroup (m a) where
  ma <> mb = do
    a <- ma
    b <- mb
    return (a <> b)

instance IsString a => IsString (Cont r a) where
  fromString = return . fromString

kvPair :: (Text, Repr r) -> Repr r
kvPair (key, Repr reprVal) = Repr $ do
  val <- reprVal
  "\"" <> return key <> "\":" <> return val

withCommas :: Monad m => [m Text] -> m Text
withCommas = fmap Text.concat . sequence . intersperse (return ",")

singleton :: Monad m => Char -> m Text
singleton = return . Text.singleton

enclose :: Char -> Cont r Text -> Char -> Cont r Text
enclose hd body tl = singleton hd <> body <> singleton tl

instance JSON (Repr r) where
  str s = Repr $ "\"" <> return s <> "\""
  num n = Repr $ return (toText n)
  bool True = Repr "true"
  bool False = Repr "false"
  null = Repr "null"
  array js = Repr $ enclose '[' (withCommas $ coerce js) ']'
  obj kvs = Repr $ enclose '{' (withCommas . coerce $ map kvPair kvs) '}'

reprS :: Repr r -> (Text -> r) -> r
reprS (Repr json) f = runCont json f

instance Show (Repr String) where
  show = flip reprS Text.unpack

instance Show (Repr Text) where
  show = Text.unpack . flip reprS id
