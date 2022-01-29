{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.Repr (
  Repr,
  text
) where


import Data.JSON (JSON(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text


toText :: Show a => a -> Text
toText = Text.pack . show


data BuilderLayer = Layer {
    queue :: [TextBuilder -> TextBuilder],
    contents :: Text,
    embed :: Text -> Text
  }

newtype TextBuilder = TextBuilder [BuilderLayer]

newtype Repr = Repr (TextBuilder -> TextBuilder)

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
surround bef aft txt = bef `Text.cons` txt `Text.snoc` aft

withSeps :: Text -> [Repr] -> [TextBuilder -> TextBuilder]
withSeps _ [] = []
withSeps _ [Repr j] = [j]
withSeps sep (Repr j : js) = (writeToLayer sep . j) : withSeps sep js

formatKV :: (Text, Repr) -> Repr
formatKV (key, Repr val) = Repr (val . writeToLayer (surround '"' '"' key <> ":"))

instance JSON Repr where
  str = Repr . writeToLayer . (surround '"' '"')
  num = Repr . writeToLayer . toText
  bool True = Repr $ writeToLayer "true"
  bool False = Repr $ writeToLayer "false"
  null = Repr $ writeToLayer "null"
  array js = Repr $ dequeue . unfoldBuilder (withSeps "," js) (surround '[' ']')
  obj props = Repr $ dequeue . unfoldBuilder (withSeps "," $ map formatKV props) (surround '{' '}')

buildText :: TextBuilder -> Text
buildText (TextBuilder []) = ""
buildText (TextBuilder [l]) = contents l
buildText tb = buildText $ foldBuilder tb

text :: Repr -> Text
text (Repr buildFrom) = buildText . buildFrom $ TextBuilder []

instance Show Repr where
  show = Text.unpack . text

instance Semigroup Repr where
  (Repr l) <> (Repr r) = Repr (l . r)

instance Monoid Repr where
  mempty = Repr id
