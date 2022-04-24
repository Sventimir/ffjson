{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Monad ((<=<))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT(..), liftEither, throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Error.Trace (Trace, ExceptTraceT, runExceptTraceT, singleError, liftTrace)
import Data.Functor ((<&>))
import Data.Input (Input(..), InputError, parseInput, loadInput)
import Data.JSON (JSON(..))
import Data.JSON.Repr (reprS)
import Data.List (replicate, reverse)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Parser.JSON (parseJSON, ParseError)
import Parser.CLI (CliArgs(..), CliError(..), FlagSpec(..), Or(..), Consume(..), cliParser)

import System.IO (IOMode(..), stdin, withFile)

import Text.Megaparsec (ParseErrorBundle)


data FFJsonError = UnexpectedPositional String
                 | InputError InputError
                 | ParseError (ParseErrorBundle Text ParseError)
                 deriving Show

instance Exception FFJsonError

data Config = Config {
    inputs :: [Input],
    indentation :: Int
  }

addInput :: Monad m => String -> Config -> ExceptTraceT m Config
addInput fname cfg = parseInput fname <&> \f -> cfg { inputs = f : inputs cfg }

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { indentation = i }

instance CliArgs (ExceptTraceT IO) Config where
  defaults = Config [] 2
  finalize cfg = return $ cfg { inputs = reverse $ inputs cfg}
  positional _ = throwM . UnexpectedPositional
  hyphens _ len = throwM . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) (\f cfg -> addInput f cfg),
            FlagSpec (OrBoth 'r' "raw") ArgZ (return . setIndentation 0),
            FlagSpec (OrRight "indent") (ArgS ArgZ) (\i -> return . setIndentation (read i))
          ]

parseJson :: (MonadIO m, JSON json) => Text -> ExceptTraceT m json
parseJson = liftTrace . parseJSON


main :: IO ()
main = do
  result <- runExceptTraceT $ do
    cfg <- cliParser defaults
    jsons <- mapM (parseJson <=< loadInput) (inputs cfg)
    return $ (cfg, array jsons)
  case result of
    Right (cfg, json) -> Text.putStrLn $ reprS json (indentation cfg) id
    Left error -> print error
