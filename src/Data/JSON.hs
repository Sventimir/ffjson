{-# LANGUAGE OverloadedStrings #-}
module Data.JSON (
  JSON(..),
  buildJSON
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
    
toText :: Show a => a -> Text
toText = pack . show

data BuilderLayer = Layer {
    queue :: [TextBuilder -> TextBuilder],
    contents :: Text,
    embed :: Text -> Text
  }

newtype TextBuilder = TextBuilder [BuilderLayer]

newtype BuildJSON = BuildJSON (TextBuilder -> TextBuilder)

instance Show BuildJSON where
  show = show . buildJSON

initialLayer :: BuilderLayer
initialLayer = Layer {
    queue = [],
    contents = "",
    embed = id
  }

foldLayer :: BuilderLayer -> Text
foldLayer l = embed l $ contents l

foldBuilder :: TextBuilder -> TextBuilder
foldBuilder (TextBuilder []) = TextBuilder []
foldBuilder (TextBuilder [l]) = TextBuilder [Layer [] (foldLayer l) id]
foldBuilder (TextBuilder (l : r : ls)) =
  TextBuilder (r { contents = contents r <> foldLayer l} : ls)

unfoldBuilder :: [TextBuilder -> TextBuilder] -> (Text -> Text) -> TextBuilder -> TextBuilder
unfoldBuilder q embed (TextBuilder ls) = TextBuilder (Layer q "" embed : ls)

dequeue :: TextBuilder -> TextBuilder
dequeue (TextBuilder []) = TextBuilder []
dequeue tb@(TextBuilder (l : ls)) = case queue l of
  [] -> foldBuilder tb
  (q : qs) -> dequeue . q $ TextBuilder (l { queue = qs } : ls)

writeToLayer :: Text -> TextBuilder -> TextBuilder
writeToLayer txt (TextBuilder []) = TextBuilder [Layer [] txt id]
writeToLayer txt (TextBuilder (l : ls)) = TextBuilder (l { contents = contents l <> txt } : ls)

surround :: Char -> Char -> Text -> Text
surround bef aft txt = bef `cons` txt `snoc` aft

withSeps :: Text -> [BuildJSON] -> [TextBuilder -> TextBuilder]
withSeps _ [] = []
withSeps _ [BuildJSON j] = [j]
withSeps sep (BuildJSON j : js) = (writeToLayer sep . j) : withSeps sep js

formatKV :: (Text, BuildJSON) -> BuildJSON
formatKV (key, BuildJSON val) = BuildJSON (val . writeToLayer (surround '"' '"' key <> ":"))

instance JSON BuildJSON where
  str = BuildJSON . writeToLayer . (surround '"' '"')
  num = BuildJSON . writeToLayer . toText
  bool True = BuildJSON $ writeToLayer "true"
  bool False = BuildJSON $ writeToLayer "false"
  null = BuildJSON $ writeToLayer "null"
  array js = BuildJSON $ dequeue . unfoldBuilder (withSeps "," js) (surround '[' ']')
  obj props = BuildJSON $ dequeue . unfoldBuilder (withSeps "," $ map formatKV props) (surround '{' '}')

buildText :: TextBuilder -> Text
buildText (TextBuilder []) = ""
buildText (TextBuilder [l]) = contents l
buildText tb = buildText $ foldBuilder tb

buildJSON :: BuildJSON -> Text
buildJSON (BuildJSON buildFrom) = buildText . buildFrom $ TextBuilder []
