module Parser.JSON (
  ParseError(..),
  parseJSON
) where

import Data.JSON (JSON(..))
import Data.Text (Text)


data ParseError = UnxpectedToken Text

parseJSON :: JSON json => Text -> Either ParseError json
parseJSON = undefined
