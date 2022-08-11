{-# LANGUAGE GADTs #-}
module Data.Input (
  Input(..),
  InputError(..),
  Inputs(Inputs),
  source,
  emptyInputs,
  isEmptyInputs,
  namedInputs,
  addInput,
  loadInput,
  parseInput
) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))

import Data.Error.Trace (ExceptTraceT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Text.Encoding (decodeUtf8)

import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP.Types

import Parser.JSON (ParseError)

import Text.Megaparsec (ParseErrorBundle)

import System.IO (IOMode(..), stdin, withFile)


data InputError = InvalidInput String
                | JsonError (ParseErrorBundle Text ParseError)
                | HttpError Text
                deriving Show

instance Exception InputError where

data Input = FileInput String | UrlInput HTTP.Request
  deriving Show

source :: Input -> String
source (FileInput fname) = fname
source (UrlInput req) = show req

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
  deriving Show

emptyInputs :: Inputs
emptyInputs = Inputs Map.empty

isEmptyInputs :: Inputs -> Bool
isEmptyInputs (Inputs m) = Map.null m

nextDefaultKey :: Inputs -> String
nextDefaultKey (Inputs m) = firstFreeKey 0 m
  where
  firstFreeKey :: Int -> Map String Input -> String
  firstFreeKey i m'
    | Map.member (show i) m' = firstFreeKey (succ i) m'
    | otherwise = show i

addInput :: Maybe String -> Input -> Inputs -> Inputs
addInput Nothing input ins@(Inputs m) =
  let key = nextDefaultKey ins in
  Inputs $ Map.insert key input m
addInput (Just key) input (Inputs m) =
  Inputs $ Map.insert key input m

namedInputs :: Inputs -> [(Text, Input)]
namedInputs (Inputs m) = map packKey $ Map.toList m
  where
  packKey :: (String, a) -> (Text, a)
  packKey (k, v) = (pack k, v)
