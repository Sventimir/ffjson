{-# LANGUAGE GADTs, RankNTypes #-}
module Data.JSON (
  JSON(..),
  JsonStream(..),
  unstream
) where
-- A tagless final form of JSON expressions.

import Data.Text (Text)


class JSON json where
  str :: Text -> json
  num :: Rational -> json
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

-- An existential wrapper to deal with polymorphic JSON values.
newtype JsonStream = JsonStream (forall json . JSON json => json)

unstream :: JSON j => JsonStream -> j
unstream (JsonStream j) = j
