{-# LANGUAGE OverloadedStrings #-}
module Data.JSON (
  JSON(..)
) where
-- A tagless final form of JSON expressions.

import Data.Text (Text,cons, snoc,  singleton, pack)


class JSON json where
  str :: Text -> json
  num :: Double -> json
  bool :: Bool -> json
  null :: json
  array :: [json] -> json
  obj :: [(Text, json)] -> json

instance (JSON a, JSON b) => JSON (a, b) where
  str txt = (str txt, str txt)
  num dbl = (num dbl, num dbl)
  bool bl = (bool bl, bool bl)
  null = (Data.JSON.null, Data.JSON.null)
  array js = let (xs, ys) = unzip js in (array xs, array ys)
  obj kvs =
    let (lbls, js) = unzip kvs
        (xs, ys) = unzip js in
    (obj (zip lbls xs), obj (zip lbls ys))
    

