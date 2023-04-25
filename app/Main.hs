{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Main where

import Control.Monad (foldM, forM_)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO)

import Data.Error.Trace (ExceptTraceT, runExceptTraceT, liftTrace, traceErrorT)
import Data.Filter (Filter)
import qualified Data.Filter as Filter
import Data.Input (Input(..), Inputs, InputError, parseInput, loadInput, emptyInputs,
                   namedInputs, addInput, isEmptyInputs, source)
import Data.JSON (JSON)
import Data.JSON.AST (toJSON)
import Data.JSON.Repr (Repr, ReprConfig(..), reprS)
import Data.JsonStream (Streamset, emptyStreamset, addStream, getStream)
import Data.Output (Output(..), parseOutput)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text

import Parser.Core (ParseError, parse, runTokenParser)
import Parser.JSON (tokJSON)
import Parser.CLI (CliArgs(..), FlagSpec(..), Or(..), Consume(..), cliParser)
import Parser.Token (tokenize)

import System.IO (IOMode(..), Handle, withFile)

import Text.Megaparsec (ParseErrorBundle)


data FFJsonError = UnexpectedPositional String
                 | InputError InputError
                 | NoInput
                 | AlreadyNamed String
                 | ParseError (ParseErrorBundle Text ParseError)
                 | Failure String
                 deriving Show

instance Exception FFJsonError

data Config = Config {
    currentInput :: Maybe (Maybe String, Input),
    inputs :: Inputs,
    filters :: [Filter],
    outputs :: [Output],
    repr :: ReprConfig
  }
  deriving Show

setIndentation :: Int -> Config -> Config
setIndentation i cfg = cfg { repr = (repr cfg) { indentationStep = i } }

enableFractionsRepr :: Config -> Config
enableFractionsRepr cfg = cfg { repr = (repr cfg) { printRationals = True } } 

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
  filt <- Filter.parseFilter $ pack code
  return $ (finalizeInput cfg) { filters = filt : filters cfg }

finalizeInput :: Config -> Config
finalizeInput cfg = case currentInput cfg of
  Nothing ->  cfg
  Just (name, input) -> cfg {
      currentInput = Nothing,
      inputs = addInput name input $ inputs cfg
    }

instance CliArgs (ExceptTraceT IO) Config where
  defaults = Config Nothing emptyInputs [] [] (ReprConfig 2 False)
  finalize cfg =
    let cfg' = finalizeInput cfg
        ins = if isEmptyInputs $ inputs cfg'
              then addInput (Just "0") (FileInput "/dev/stdin") emptyInputs
              else inputs cfg'
        outs = if null $ outputs cfg'
               then [Output "0" "/dev/stdout"]
               else outputs cfg' in
    return $ cfg' {
        inputs = ins,
        filters = reverse $ filters cfg',
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
            FlagSpec (OrRight "ratios") ArgZ (return . enableFractionsRepr),
            FlagSpec
              (OrRight "indent")
              (ArgS ArgZ)
              (\i -> return . setIndentation (read i) . finalizeInput)
          ]

main :: IO ()
main = do
  result <- runExceptTraceT $ do
    cfg <- traceErrorT (Failure "Wrong command line arguments") $ cliParser defaults
    jsons <- foldM readJson emptyStreamset . namedInputs $ inputs cfg
    jsons' <- foldM evalFilter jsons $ filters cfg
    return (cfg, jsons')
  case result of
    Right (cfg, json) -> forM_ (outputs cfg) (outputJson (repr cfg) json)
    Left err -> print err

readJson :: Streamset -> (Text, Input) -> ExceptTraceT IO Streamset
readJson streams (k, input) =
    traceErrorT (Failure $ "Could not parse JSON from: " ++ source input) $ do
      v <- loadInput input >>= parseJson (source input)
      return $ addStream k (toJSON v) streams

parseJson :: (JSON j, MonadIO m) => String -> Text -> ExceptTraceT m j
parseJson src json = do
  tokens <- liftTrace $ parse tokenize src json
  runTokenParser (fix tokJSON) tokens

outputJson :: ReprConfig -> Streamset -> Output -> IO ()
outputJson cfg streamset (Output key filename) =
   case getStream key streamset of
     Nothing -> return ()
     Just json -> withFile filename WriteMode . write cfg $ toJSON json

write :: ReprConfig -> Repr Text -> Handle -> IO ()
write cfg json fd = Text.hPutStrLn fd (reprS json cfg id)

evalFilter :: Streamset -> Filter -> ExceptTraceT IO Streamset
evalFilter j f = traceErrorT
                   (Failure "Filter execution failed!")
                   (liftTrace $ Filter.evaluate f j)
