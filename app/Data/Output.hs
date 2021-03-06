module Data.Output (
  Output(..),
  parseOutput
) where

import Data.List (intercalate)
import Data.Text (Text, pack)

-- Text argument represents a key in the streamset.
data Output = Output Text FilePath


parseOutput :: String -> Output
parseOutput txt = case strSplit ':' txt of
  -- never returns an empty list
  [] -> error "impossible"
  [key] -> Output (pack key) "/dev/stdout"
  (key : filename) ->  Output (pack key) (intercalate ":" filename)

strSplit :: Char -> String -> [String]
strSplit sep input = reverse $ doSplit [""] input
  where
  doSplit :: [String] -> String -> [String]
  doSplit acc [] = acc
  doSplit [] (_ : _) = error "impossible" -- always called with non-empty acc
  doSplit (cur : acc) (chr : rm)
    | chr == sep = doSplit ("" : reverse cur : acc) rm
    | otherwise = doSplit ((chr : cur) : acc) rm
