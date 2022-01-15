module Main where

import Data.JSON (buildJSON)
import Parser.JSON (parseJSON)

import qualified Data.Text.IO as Text
import System.IO (stdin)


main :: IO ()
main = do
  input <- Text.hGetContents stdin
  case parseJSON input of
    Right json -> Text.putStrLn $ buildJSON json
    Left error -> print error
