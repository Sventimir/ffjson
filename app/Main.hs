{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Monad ((<=<))
import Control.Monad.Catch (Exception)
import Control.Monad.Except (ExceptT(..), liftEither, throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Error.Trace (ETrace, TracedExceptT, throw, singleError)
import Data.Functor ((<&>))
import Data.Input (Input(..), InputError, Filename)
import Data.JSON (JSON(..))
import Data.JSON.Repr (reprS)
import Data.List (replicate, reverse)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Parser.JSON (parseJSON, ParseError)
import Parser.CLI (CliArgs(..), CliError(..), FlagSpec(..), Or(..), Consume(..), cliParser)

import System.IO (IOMode(..), stdin, withFile)

import Text.Megaparsec (ParseErrorBundle)


data FFJsonError = UnrecognisedLong String
                 | UnrecognisedShort Char
                 | MissingParam String
                 | UnexpectedPositional String
                 | InputError InputError
                 deriving Show

instance Exception FFJsonError

instance CliError ETrace where
  unrecognisedLong = singleError . UnrecognisedLong
  unrecognisedShort = singleError . UnrecognisedShort
  missingParam = singleError . MissingParam

newtype Inputs = Inputs [String]

data Config = Config {
    inputs :: [Filename],
    indentation :: Int
  }

addInput :: Monad m => String -> Config -> TracedExceptT m Config
addInput fname cfg = parseInput fname <&> \f -> cfg { inputs = f : inputs cfg }

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { indentation = i }

instance CliArgs ETrace Config where
  defaults = Config [] 2
  finalize cfg = return $ cfg { inputs = reverse $ inputs cfg}
  positional _ = throw . UnexpectedPositional
  hyphens _ len = throw . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) (\f cfg -> addInput f cfg),
            FlagSpec (OrBoth 'r' "raw") ArgZ (return . setIndentation 0),
            FlagSpec (OrRight "indent") (ArgS ArgZ) (\i -> return . setIndentation (read i))
          ]


main :: IO ()
main = do
  result <- runExceptT $ do
    cfg <- cliParser defaults
    jsons <- mapM loadInput (inputs cfg)
    return $ (cfg, array jsons)
  case result of
    Right (cfg, json) -> Text.putStrLn $ reprS json (indentation cfg) id
    Left error -> print error

