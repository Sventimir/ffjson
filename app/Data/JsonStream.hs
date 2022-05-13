{-# LANGUAGE GADTs, RankNTypes #-}
module Data.JsonStream (
  Streamset,
  emptyStreamset,
  addStream,getStream,
  toObject
) where

import Data.Input (Input)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)


data Streamset = Streamset (Map Text JsonAst)

emptyStreamset :: Streamset
emptyStreamset = Streamset Map.empty

addStream :: Text -> JsonAst -> Streamset -> Streamset
addStream name json (Streamset m) = Streamset $ Map.insert name json m

getStream :: Text -> Streamset -> Maybe JsonAst
getStream name (Streamset m) = Map.lookup name m

toObject :: Streamset -> JsonAst
toObject (Streamset m) = obj $ Map.toList m
