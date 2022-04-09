module Data.Input (
  Input(..),
  InputError(..),
  Filename
) where

import Control.Monad.Except (MonadError, ExceptT, withExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Error.Trace (TracedExceptT, addError)
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
  parseInput :: Monad m => String -> TracedExceptT m i
  loadInput :: (MonadIO m, JSON j) => i -> TracedExceptT m j


newtype Filename = Filename String

instance Input Filename where
  parseInput = return . Filename
  loadInput (Filename f) = loadFile f >>= parseJson

loadFile :: MonadIO m => String -> TracedExceptT m Text
loadFile "-" = liftIO $ Text.hGetContents stdin
loadFile fname = liftIO $ withFile fname ReadMode Text.hGetContents

parseJson :: (MonadIO m, JSON json) => Text -> TracedExceptT m json
parseJson = liftEither . parseJSON
