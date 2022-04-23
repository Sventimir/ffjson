module Data.Input (
  Input(..),
  InputError(..),
  Filename
) where

import Control.Monad.Except (MonadError, ExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Error.Trace (ExceptTraceT, liftTrace)
import Data.JSON (JSON)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Parser.JSON (ParseError, parseJSON)

import Text.Megaparsec (ParseErrorBundle)

import System.IO (IOMode(..), stdin, withFile)


data InputError = InvalidInput String
                | JsonError (ParseErrorBundle Text ParseError)
                deriving Show

class Input i where
  parseInput :: Monad m => String -> ExceptTraceT m i
  loadInput :: (MonadIO m, JSON j) => i -> ExceptTraceT m j


newtype Filename = Filename String

instance Input Filename where
  parseInput = return . Filename
  loadInput (Filename f) = loadFile f >>= parseJson

loadFile :: MonadIO m => String -> ExceptTraceT m Text
loadFile "-" = liftIO $ Text.hGetContents stdin
loadFile fname = liftIO $ withFile fname ReadMode Text.hGetContents

parseJson :: (MonadIO m, JSON json) => Text -> ExceptTraceT m json
parseJson = liftTrace . parseJSON
