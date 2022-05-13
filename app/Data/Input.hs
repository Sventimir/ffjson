{-# LANGUAGE GADTs #-}
module Data.Input (
  Input(..),
  InputError(..),
  Inputs(Inputs),
  emptyInputs,
  isEmptyInputs,
  namedInputs,
  addInput,
  loadInput,
  parseInput
) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Error.Trace (ExceptTraceT, liftTrace)
import Data.JSON (JSON)
import Data.List (minimum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
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


newtype Inputs = Inputs (Map String Input)

emptyInputs :: Inputs
emptyInputs = Inputs Map.empty

isEmptyInputs :: Inputs -> Bool
isEmptyInputs (Inputs m) = Map.null m

nextDefaultKey :: Inputs -> String
nextDefaultKey (Inputs m) = firstFreeKey 0 m
  where
  firstFreeKey :: Int -> Map String Input -> String
  firstFreeKey i m
    | Map.member (show i) m = firstFreeKey (succ i) m
    | otherwise = show i

addInput :: Maybe String -> Input -> Inputs -> Inputs
addInput Nothing input ins@(Inputs map) =
  let key = nextDefaultKey ins in
  Inputs $ Map.insert key input map
addInput (Just key) input (Inputs map) =
  Inputs $ Map.insert key input map

namedInputs :: Inputs -> [(Text, Input)]
namedInputs (Inputs m) = map packKey $ Map.toList m
  where
  packKey :: (String, a) -> (Text, a)
  packKey (k, v) = (pack k, v)
