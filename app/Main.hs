{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Main where

import Control.Monad (foldM)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT(..), liftEither, throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Error.Trace (Trace, ExceptTraceT, runExceptTraceT, singleError, liftTrace)
import Data.Functor ((<&>))
import Data.Input (Input(..), Inputs, InputError, parseInput, loadInput, emptyInputs,
                   namedInputs, addInput)
import Data.JSON (JSON(..), JsonStream(..))
import Data.JSON.AST (JsonAst, toJSON)
import Data.JSON.Repr (reprS)
import Data.JsonStream (Streamset, emptyStreamset, addStream, toObject)
import Data.List (replicate, reverse)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Parser.JSON (parseJSON, ParseError)
import Parser.CLI (CliArgs(..), CliError(..), FlagSpec(..), Or(..), Consume(..), cliParser)

import System.IO (IOMode(..), stdin, withFile)

import Text.Megaparsec (ParseErrorBundle)


data FFJsonError = UnexpectedPositional String
                 | InputError InputError
                 | NoInput
                 | AlreadyNamed String
                 | ParseError (ParseErrorBundle Text ParseError)
                 deriving Show

instance Exception FFJsonError

data Config = Config {
    currentInput :: Maybe (Maybe String, Input),
    inputs :: Inputs,
    indentation :: Int
  }

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { indentation = i }

addNewInput :: String -> Config -> ExceptTraceT IO Config
addNewInput flag cfg = do
  inp <- parseInput flag
  return $ (finalizeInput cfg) { currentInput = Just (Nothing, inp) }

setInputName :: String -> Config -> ExceptTraceT IO Config
setInputName name cfg = case currentInput cfg of
  Nothing -> throwM NoInput
  Just (Nothing, inp) -> return $ cfg { currentInput = Just (Just name, inp) }
  Just (Just n, _) -> throwM $ AlreadyNamed n

finalizeInput :: Config -> Config
finalizeInput cfg = case currentInput cfg of
  Nothing ->  cfg
  Just (name, input) -> cfg {
      currentInput = Nothing,
      inputs = addInput name input $ inputs cfg
    }

instance CliArgs (ExceptTraceT IO) Config where
  defaults = Config Nothing emptyInputs 2
  finalize = return . finalizeInput
  positional _ = throwM . UnexpectedPositional
  hyphens _ len = throwM . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) (\f cfg -> addNewInput f cfg),
            FlagSpec (OrBoth 'n' "name") (ArgS ArgZ) setInputName,
            FlagSpec (OrBoth 'r' "raw") ArgZ (return . setIndentation 0 . finalizeInput),
            FlagSpec
              (OrRight "indent")
              (ArgS ArgZ)
              (\i -> return . setIndentation (read i) . finalizeInput)
          ]

main :: IO ()
main = do
  result <- runExceptTraceT $ do
    cfg <- cliParser defaults
    jsons <- foldM readJson emptyStreamset . namedInputs $ inputs cfg
    return $ (cfg, toObject jsons)
  case result of
    Right (cfg, json) -> Text.putStrLn $ reprS json (indentation cfg) id
    Left error -> print error

readJson :: Streamset -> (Text, Input) -> ExceptTraceT IO Streamset
readJson streams (k, input) = do
  v <- loadInput input >>= parseJson
  return $ addStream k (toJSON v) streams

parseJson :: MonadIO m => Text -> (forall j. JSON j => ExceptTraceT m j)
parseJson = liftTrace . parseJSON

