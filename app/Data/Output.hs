module Data.Output (
  Output(..),
  parseOutput
) where

import Data.List (intercalate)
import Data.Text (Text, pack)
import System.IO (FilePath)

-- Text argument represents a key in the streamset.
data Output = Output Text FilePath


parseOutput :: String -> Output
parseOutput txt = case strSplit ':' txt of
  -- never returns an empty list
  [key] -> Output (pack key) "/dev/stdout"
  (key : filename) ->  Output (pack key) (intercalate ":" filename)

strSplit :: Char -> String -> [String]
strSplit sep input = reverse $ doSplit [""] input
  where
  doSplit :: [String] -> String -> [String]
  doSplit acc [] = acc
  doSplit (cur : acc) (chr : rem)
    | chr == sep = doSplit ("" : reverse cur : acc) rem
    | otherwise = doSplit ((chr : cur) : acc) rem
