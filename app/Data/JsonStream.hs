{-# LANGUAGE GADTs, RankNTypes #-}
module Data.JsonStream (
  Streamset,
  emptyStreamset,
  addStream,getStream,
  toObject
) where

import Data.Input (Input)
import Data.JSON (JSON(..), JsonStream(..), unstream)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)


data Streamset = Streamset (Map Text JsonStream)

emptyStreamset :: Streamset
emptyStreamset = Streamset Map.empty

addStream :: Text -> (forall j. JSON j => j) -> Streamset -> Streamset
addStream name json (Streamset m) = Streamset $ Map.insert name (JsonStream json) m

getStream :: JSON j => Text -> Streamset -> Maybe j
getStream name (Streamset m) = fmap unstream $ Map.lookup name m

toObject :: JSON j => Streamset -> j
toObject (Streamset m) = obj . Map.toList $ Map.map unstream m
