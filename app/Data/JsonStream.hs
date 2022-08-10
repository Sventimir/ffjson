{-# LANGUAGE GADTs, RankNTypes #-}
module Data.JsonStream (
  Streamset,
  emptyStreamset,
  addStream,
  getStream,
  getStreams,
  toObject
) where

import Control.Monad (foldM)
import Control.Monad.Catch (Exception(..))

import Data.Error.Trace (EitherTrace, eitherJustTrace)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)


newtype Streamset = Streamset (Map Text JsonAst)

newtype StreamError = UnknownStream Text
  deriving Show

instance Exception StreamError where
  

emptyStreamset :: Streamset
emptyStreamset = Streamset Map.empty

addStream :: Text -> JsonAst -> Streamset -> Streamset
addStream name json (Streamset m) = Streamset $ Map.insert name json m

getStream :: Text -> Streamset -> Maybe JsonAst
getStream name (Streamset m) = Map.lookup name m

getStreams :: [Text] -> Streamset -> EitherTrace [(Text, JsonAst)]
getStreams ks streamset = reverse <$> foldM addStream [] ks
  where
  addStream :: [(Text, JsonAst)] -> Text -> EitherTrace [(Text, JsonAst)]
  addStream acc k = ((: acc) . (,) k)
                    <$> eitherJustTrace (UnknownStream k) (getStream k streamset)

toObject :: Streamset -> JsonAst
toObject (Streamset m) = obj $ Map.toList m
