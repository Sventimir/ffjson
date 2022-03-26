module Main where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, liftEither, throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.JSON (JSON(..))
import Data.JSON.Repr (reprS)
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

data Config = Config {
    inputs :: [String],
    indentation :: Int
  }

addInput :: String -> Config -> Config
addInput filename cfg = cfg { inputs = filename : inputs cfg}

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { indentation = i }

instance CliArgs Config where
  defaults = Config [] 2
  finalize cfg = return $ cfg { inputs = reverse $ inputs cfg}
  positional _ = throwError . UnexpectedPositional
  hyphens _ len = throwError . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) (\f -> return . addInput f),
            FlagSpec (OrBoth 'r' "raw") ArgZ (return . setIndentation 0),
            FlagSpec (OrRight "indent") (ArgS ArgZ) (\i -> return . setIndentation (read i))
          ]


main :: IO ()
main = do
  result <- runExceptT $ do
    cfg <- withExceptT CliError $ cliParser defaults
    jsons <- mapM (parseJson <=< loadFile) (inputs cfg)
    return $ (cfg, array jsons)
  case result of
    Right (cfg, json) -> Text.putStrLn $ reprS json (indentation cfg) id
    Left error -> print error

loadFile :: MonadIO m => String -> ExceptT FFJsonError m Text
loadFile "-" = liftIO $ Text.hGetContents stdin
loadFile fname = liftIO $ withFile fname ReadMode Text.hGetContents

parseJson :: (MonadIO m, JSON json) => Text -> ExceptT FFJsonError m json
parseJson = withExceptT JsonError . liftEither . parseJSON
