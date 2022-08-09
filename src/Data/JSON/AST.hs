{-# LANGUAGE RankNTypes #-}
module Data.JSON.AST (
  JsonAst(..),
  TypeError(..),
  ValueError(..),
  toJSON,
  expectArray,
  expectObject,
  expectNumber,
  expectBool,
  cmpr
) where

import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Error.Trace (EitherTrace)
import Data.JSON (JSON)
import qualified Data.JSON as JSON
import Data.JSON.Repr (Repr)
import Data.Text (Text)


data JsonAst = JString Text
             | JNum Rational
             | JBool Bool
             | JNull
             | JArray [JsonAst]
             | JObject [(Text, JsonAst)]
             deriving Eq

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

instance Show JsonAst where
  show j = show (toJSON j :: Repr String)

data TypeError = NotAnObject JsonAst
               | NotAnArray JsonAst
               | NotANumber JsonAst
               | NotABoolean JsonAst
               | NotSized JsonAst

instance Show TypeError where
  show (NotAnObject j) = "Not an object: '" ++ show j ++ "'!"
  show (NotAnArray j) = "Not an array: '" ++ show j ++ "'!"
  show (NotANumber j) = "Not a number: '" ++ show j ++ "'!"
  show (NotABoolean j) = "Not a boolean value: '" ++ show j ++ "'!"
  show (NotSized j) = "'" ++ show j ++ "' does not have any size!"

instance Exception TypeError where

data ValueError = NegativeIndex Int
                | ZeroDivision
                | Incomparable JsonAst (Maybe JsonAst)

instance Show ValueError where
  show (NegativeIndex i) = "Negative array index: " ++ show i ++ "!"
  show ZeroDivision = "Cannot divide by zero!"
  show (Incomparable j Nothing) = "Value '" ++ show j ++ "' is incmprarable!"
  show (Incomparable j (Just k)) = "Values '" ++ show j ++ "' and '"
                                   ++ show k ++ "' cannot be compared!"

instance Exception ValueError


expectArray :: JsonAst -> EitherTrace [JsonAst]
expectArray (JArray js) = return js
expectArray j = throwM $ NotAnArray j

expectObject :: JsonAst -> EitherTrace [(Text, JsonAst)]
expectObject (JObject kvs) = return kvs
expectObject j = throwM $ NotAnObject j

expectNumber :: JsonAst -> EitherTrace Rational
expectNumber (JNum n) = return n
expectNumber j = throwM $ NotANumber j

expectBool :: JsonAst -> EitherTrace Bool
expectBool (JBool b) = return b
expectBool j = throwM $ NotABoolean j

cmpr :: JsonAst -> JsonAst -> EitherTrace Ordering
cmpr (JString a) (JString b) = return $ compare a b
cmpr (JNum a) (JNum b) = return $ compare a b
cmpr (JBool a) (JBool b) = return $ compare a b
cmpr arr@(JArray _) _ = throwM $ Incomparable arr Nothing
cmpr obj@(JObject _) _ = throwM $ Incomparable obj Nothing
cmpr _ arr@(JArray _) = throwM $ Incomparable arr Nothing
cmpr _ obj@(JObject _) = throwM $ Incomparable obj Nothing
cmpr JNull JNull = return EQ
cmpr JNull _ = return LT
cmpr _ JNull = return GT
cmpr a b = throwM $ Incomparable a (Just b)
