{-# LANGUAGE RankNTypes #-}
module Data.JSON.AST (
  JsonAst(..),
  ObjectError(..),
  toJSON
) where

import Control.Monad.Catch (Exception)
import Data.JSON (JSON)
import qualified Data.JSON as JSON
import Data.JSON.Repr (Repr)
import Data.Text (Text)


data JsonAst = JString Text
             | JNum Double
             | JBool Bool
             | JNull
             | JArray [JsonAst]
             | JObject [(Text, JsonAst)]

instance JSON JsonAst where
  str = JString
  num = JNum
  bool = JBool
  null = JNull
  array = JArray
  obj = JObject

toJSON :: JsonAst -> (forall j. JSON j => j)
toJSON (JString s) = JSON.str s
toJSON (JNum n) = JSON.num n
toJSON (JBool b) = JSON.bool b
toJSON JNull = JSON.null
toJSON (JArray js) = JSON.array $ map toJSON js
toJSON (JObject kvs) = JSON.obj $ map pairToJSON kvs
  where
  pairToJSON (k, v) = (k, toJSON v)

data ObjectError = NotAnObject JsonAst

instance Show ObjectError where
  show (NotAnObject j) = "Not an object: '" ++ show (toJSON j :: Repr String) ++ "'!"

instance Exception ObjectError where
