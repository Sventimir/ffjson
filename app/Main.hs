module Main where

import Data.JSON.Repr (text)
import Parser.JSON (parseJSON)

import qualified Data.Text.IO as Text
import System.IO (stdin)


main :: IO ()
main = do
  input <- Text.hGetContents stdin
  case parseJSON input of
    Right json -> Text.putStrLn $ text json
    Left error -> print error
