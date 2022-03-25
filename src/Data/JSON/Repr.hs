{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Data.JSON.Repr (
  reprS
) where

import Control.Monad.Trans.Cont (Cont, runCont)

import Data.Coerce (coerce)
import Data.JSON (JSON(..))
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

instance JSON (Repr r) where
  str s = Repr $ "\"" <> return s <> "\""
  num n = Repr $ return (toText n)
  bool True = Repr "true"
  bool False = Repr "false"
  null = Repr "null"
  array js = Repr $ do
      arr <- sequence $ coerce js
      "[" <> return (Text.intercalate "," arr) <> "]"
  obj kvs = Repr $ do
      o <- sequence . coerce $ map kvPair kvs
      "{" <> return (Text.intercalate "," o) <> "}"

reprS :: Repr r -> (Text -> r) -> r
reprS (Repr json) f = runCont json f

instance Show (Repr String) where
  show = flip reprS Text.unpack

instance Show (Repr Text) where
  show = Text.unpack . flip reprS id
