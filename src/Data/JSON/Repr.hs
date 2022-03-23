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

kvPair :: (Text, Repr r) -> Repr r
kvPair (key, Repr reprVal) = Repr $ do
  val <- reprVal
  return ("\"" <> key <> "\":" <> val)

instance JSON (Repr r) where
  str s = Repr $ return ("\"" <> s <> "\"")
  num n = Repr $ return (toText n)
  bool True = Repr $ return "true"
  bool False = Repr $ return "false"
  null = Repr $ return "null"
  array js = Repr $ do
      arr <- sequence $ coerce js
      return ("[" <> Text.intercalate "," arr <> "]")
  obj kvs = Repr $ do
      o <- sequence . coerce $ map kvPair kvs
      return ("{" <> Text.intercalate "," o <> "}")

reprS :: Repr r -> (Text -> r) -> r
reprS (Repr json) f = runCont json f

instance Show (Repr String) where
  show = flip reprS Text.unpack

instance Show (Repr Text) where
  show = Text.unpack . flip reprS id
