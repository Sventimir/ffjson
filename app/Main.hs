module Main where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, liftEither, throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.JSON (JSON(..))
import Data.JSON.Repr (text)
import Data.List (replicate, reverse)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Parser.JSON (parseJSON, ParseError)
import Parser.CLI (CliArgs(..), CliError(..), FlagSpec(..), Or(..), Consume(..), cliParser)

import System.IO (IOMode(..), stdin, withFile)

import Text.Megaparsec (ParseErrorBundle)


data FFJsonError = CliError CliError
                 | JsonError (ParseErrorBundle Text ParseError)
                 deriving Show

newtype Inputs = Inputs [String]

addInput :: String -> Inputs -> Inputs
addInput filename (Inputs ins) = Inputs (filename : ins)

instance CliArgs Inputs where
  defaults = Inputs []
  finalize (Inputs ins) = return $ Inputs (reverse ins)
  positional _ = throwError . UnexpectedPositional
  hyphens _ len = throwError . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) (\f -> return . addInput f)
          ]


main :: IO ()
main = do
  result <- runExceptT $ do
    Inputs inputs <- withExceptT CliError $ cliParser defaults
    jsons <- mapM (parseJson <=< loadFile) inputs
    return $ array jsons
  case result of
    Right json -> Text.putStrLn $ text json
    Left error -> print error

loadFile :: MonadIO m => String -> ExceptT FFJsonError m Text
loadFile "-" = liftIO $ Text.hGetContents stdin
loadFile fname = liftIO $ withFile fname ReadMode Text.hGetContents

parseJson :: (MonadIO m, JSON json) => Text -> ExceptT FFJsonError m json
parseJson = withExceptT JsonError . liftEither . parseJSON
