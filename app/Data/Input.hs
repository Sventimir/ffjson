{-# LANGUAGE GADTs #-}
module Data.Input (
  Input(..),
  InputError(..),
  loadInput,
  parseInput
) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Error.Trace (ExceptTraceT, liftTrace)
import Data.JSON (JSON)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Text.Encoding (decodeUtf8)

import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP.Types

import Parser.JSON (ParseError, parseJSON)

import Text.Megaparsec (ParseErrorBundle)

import System.IO (IOMode(..), stdin, withFile)


data InputError = InvalidInput String
                | JsonError (ParseErrorBundle Text ParseError)
                | HttpError Text
                deriving Show

instance Exception InputError where

data Input = FileInput String | UrlInput HTTP.Request

parseInput :: Monad m => String -> ExceptTraceT m Input
parseInput input = fmap UrlInput (HTTP.parseRequest input)
                   <|> return (FileInput input)

loadInput :: MonadIO m => Input -> ExceptTraceT m Text
loadInput (FileInput fname) = loadFile fname
loadInput (UrlInput req) = do
  resp <- liftIO $ HTTP.httpBS req
  let body = decodeUtf8 $ HTTP.getResponseBody resp
  if HTTP.Types.statusIsSuccessful $ HTTP.getResponseStatus resp
    then return body
    else throwM $ HttpError body

loadFile :: MonadIO m => String -> ExceptTraceT m Text
loadFile "-" = liftIO $ Text.hGetContents stdin
loadFile fname = liftIO $ withFile fname ReadMode Text.hGetContents

