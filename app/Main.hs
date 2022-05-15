{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Main where

import Control.Monad (foldM, forM_)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Except (ExceptT(..), throwError, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Error.Trace (Trace, ExceptTraceT, runExceptTraceT, singleError, liftTrace)
import Data.Filter (Filter)
import qualified Data.Filter as Filter
import Data.Functor ((<&>))
import Data.Input (Input(..), Inputs, InputError, parseInput, loadInput, emptyInputs,
                   namedInputs, addInput, isEmptyInputs)
import Data.JSON (JSON, JsonStream(..))
import Data.JSON.AST (JsonAst, toJSON)
import Data.JSON.Repr (Repr, reprS)
import Data.JsonStream (Streamset, emptyStreamset, addStream, getStream, toObject)
import Data.List (replicate, reverse)
import Data.Output (Output(..), parseOutput)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text

import Language.Core (Composable(..))
import Language.Eval (Eval, eval)

import Parser.JSON (parseJSON, ParseError)
import Parser.CLI (CliArgs(..), CliError(..), FlagSpec(..), Or(..), Consume(..), cliParser)

import System.IO (FilePath, IOMode(..), Handle, stdin, withFile)

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
    filters :: [Filter],
    outputs :: [Output],
    indentation :: Int
  }

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { indentation = i }

addOutput :: String -> Config -> Config
addOutput out cfg = (finalizeInput cfg) { outputs = parseOutput out : outputs cfg }

addNewInput :: String -> Config -> ExceptTraceT IO Config
addNewInput flag cfg = do
  inp <- parseInput flag
  return $ (finalizeInput cfg) { currentInput = Just (Nothing, inp) }

setInputName :: String -> Config -> ExceptTraceT IO Config
setInputName name cfg = case currentInput cfg of
  Nothing -> throwM NoInput
  Just (Nothing, inp) -> return $ cfg { currentInput = Just (Just name, inp) }
  Just (Just n, _) -> throwM $ AlreadyNamed n

addFilter :: String -> Config -> ExceptTraceT IO Config
addFilter code cfg = do
  filter <- liftTrace . Filter.parse $ pack code
  return $ (finalizeInput cfg) { filters = filter : filters cfg }

finalizeInput :: Config -> Config
finalizeInput cfg = case currentInput cfg of
  Nothing ->  cfg
  Just (name, input) -> cfg {
      currentInput = Nothing,
      inputs = addInput name input $ inputs cfg
    }

instance CliArgs (ExceptTraceT IO) Config where
  defaults = Config Nothing emptyInputs [] [] 2
  finalize cfg =
    let cfg' = finalizeInput cfg
        ins = if isEmptyInputs $ inputs cfg'
              then addInput (Just "0") (FileInput "/dev/stdin") emptyInputs
              else inputs cfg'
        outs = if null $ outputs cfg'
               then [Output "0" "/dev/stdout"]
               else outputs cfg' in
    return $ (finalizeInput cfg) {
        inputs = ins,
        filters = reverse $ filters cfg,
        outputs = outs
      }
  positional _ = throwM . UnexpectedPositional
  hyphens _ len = throwM . UnexpectedPositional $ replicate len '-'
  flags = [
            FlagSpec (OrBoth 'i' "input") (ArgS ArgZ) addNewInput,
            FlagSpec (OrBoth 'n' "name") (ArgS ArgZ) setInputName,
            FlagSpec (OrBoth 'o' "output") (ArgS ArgZ) (\f cfg -> return $ addOutput f cfg),
            FlagSpec (OrBoth 'r' "raw") ArgZ (return . setIndentation 0 . finalizeInput),
            FlagSpec (OrBoth 'f' "filter") (ArgS ArgZ) addFilter,
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
    jsons' <- foldM (\j f -> liftTrace $ Filter.evaluate f j) jsons $ filters cfg
    return $ (cfg, jsons')
  case result of
    Right (cfg, json) -> forM_ (outputs cfg) (outputJson (indentation cfg) json)
    Left error -> print error

readJson :: Streamset -> (Text, Input) -> ExceptTraceT IO Streamset
readJson streams (k, input) = do
  v <- loadInput input >>= parseJson
  return $ addStream k (toJSON v) streams

parseJson :: (JSON j, MonadIO m) => Text -> ExceptTraceT m j
parseJson = liftTrace . parseJSON

outputJson :: Int -> Streamset -> Output -> IO ()
outputJson indent streamset (Output key filename) =
   case getStream key streamset of
     Nothing -> return ()
     Just json -> withFile filename WriteMode . write indent $ toJSON json

write :: Int -> Repr Text -> Handle -> IO ()
write indent json fd = Text.hPutStrLn fd (reprS json indent id)
