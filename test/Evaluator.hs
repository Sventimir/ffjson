{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Evaluator ( evalTests ) where

import Prelude hiding (null)
import Test.Hspec

import Control.Monad (foldM)
import Control.Monad.Catch (SomeException, fromException)
import Control.Monad.Fix (fix)

import Data.Error.Trace (ExceptTraceT, liftTrace, runToIO)
import Data.JSON (JSON(..))
import Data.JSON.AST (JsonAst(..), TypeError(..), ValueError(..))
import Data.Text (Text)

import Language.Eval
import Parser.Core (TokParseError(..), parse, runTokenParser, eof)
import Parser.Token (Token, tokenize)
import qualified Parser.JSON as JsonParser
import Parser.Language (tokExpr)


evalTests :: Spec
evalTests = do
  describe "Test escaping quotation marks in strings." $ do
    it "A quotation mark signals an end of a string." $
      "\"some \"string\"\"" `appliedTo` "id" `shouldThrow` anyException
    it "A properly escaped quotation mark does not end a string." $
      "concat id \" found\\\"\"" `appliedTo` "\"\\\"not\""
        `shouldReturn` str "\"not found\""
    it "Backslash should also be escaped." $
      "id" `appliedTo` "\"\\\\\"" `shouldReturn` str "\\"
  describe "Test simple object getter." $ do
    it "Get existing key." $
      ".a" `appliedTo` "{\"a\": 1}" `shouldReturn` num 1
    it "Non-existent key returns `null`." $
      ".a" `appliedTo` "{}" `shouldReturn` null
    it "Get on non-object term is an error" $
      ".a" `appliedTo` "[]" `shouldThrow` notAnObject (array [])
  describe "Object getters compose." $ do
    it "Get from a nested object" $
      ".a.b" `appliedTo` "{\"a\": {\"b\": 1234}}" `shouldReturn` num 1234
  describe "Test array getter." $ do
    it "Get from array by existing index." $
      ".[0]" `appliedTo` "[1, 2, 3]" `shouldReturn` num 1
    it "Get from array by non-exitent index." $
      ".[0]" `appliedTo` "[]" `shouldReturn` null
    it "Array getters compose." $
      ".[1].[0]" `appliedTo` "[0, [1, 2, 3], {}]" `shouldReturn` num 1
    it "Get from non-array fails" $
      ".[0]" `appliedTo` "{}" `shouldThrow` notAnArray (obj [])
    it "Get from array by negative index counts elements from the end of the array." $
      ".[-1]" `appliedTo` "[1, 2, 3]" `shouldReturn` num 3
  describe "Array and objects getters compose together." $ do
    it "Compose array and object getter." $
      ".a.[0].b" `appliedTo` "{\"a\": [{\"b\": true}, 1], \"z\": null}" `shouldReturn` bool True
    it "Array and object getters don't compose if separated by spaces." $
      ".a .[0] .b" `appliedTo` "{\"a\": [{\"b\": true}, 1], \"z\": null}" `shouldThrow` parseError
  describe "Test `keys` function in isolation" $ do
    it "`keys` returns a list a of keys of an object." $
      "keys id" `appliedTo` "{}" `shouldReturn` array []
    it "keys returned by `keys` appear in order of definition." $
      "keys id" `appliedTo` "{\"a\": 1, \"c\": 3, \"b\": 2}" `shouldReturn` array [str "a", str "c", str "b"]
    it "`keys` applied to non-object fails." $
      "keys id" `appliedTo` "[]" `shouldThrow` notAnObject (array [])
  describe "Test filter composition." $ do
    it "Get from keys list." $
      "keys id | .[0]" `appliedTo` "{\"aaa\": [], \"zzz\": 12}" `shouldReturn` str "aaa"
    it "Use compose function instead of operator." $
      "compose (keys id) .[0]" `appliedTo` "{\"a\": null}" `shouldReturn` str "a"
  describe "Test parenthesised sub-expressions." $ do
    it "Parentheses enclose expressions." $ 
      "(keys id)" `appliedTo` "{}" `shouldReturn` array []
    it "Parentheses separate sub-expressions too." $
      "(.[0] | .x) | keys id" `appliedTo` "[{\"x\": {}}]" `shouldReturn` array []
    it "Parentheses can be nested" $
      ".x.a | ((keys id) | .[0])" `appliedTo` "{\"x\": {\"a\": {\"z\": 123}}}"
        `shouldReturn` str "z"
    it "A failure inside parentheses also causes an error" $
      "(keys)" `appliedTo` "{}" `shouldThrow` parseError
  describe "Test syntax in conjunction with standard JSON." $ do
    it "Complex expressions in dictionary." $
      "{\"a\": keys .x, \"b\": .y | .[0]}" `appliedTo` "{\"x\": {}, \"y\": [1]}"
        `shouldReturn` obj [("a", array []), ("b", num 1)]
  describe "Test arithmetic" $ do
    it "Add two properties of an object." $
      ".a + .b" `appliedTo` "{\"a\": 1, \"b\": 2}" `shouldReturn` num 3
    it "Add a property to a constant" $
      ".a + 3" `appliedTo` "{\"a\": 0}" `shouldReturn` num 3
    it "Chain more additions together" $ do
      ".[0] + .[1] + .[2]" `appliedTo` "[4, 5, 6]" `shouldReturn` num 15
    it "Only numbers add." $
      ".a + 3" `appliedTo` "{\"a\": null}" `shouldThrow` notANumber null
    it "Subtraction is addition with second argument negated." $
      ".a - .b" `appliedTo` "{\"a\": 3, \"b\": 2}" `shouldReturn` num 1
    it "Division is multipilcation with second argument inverted." $
      ".a / .b" `appliedTo` "{\"a\": 3, \"b\": 4}" `shouldReturn` num 0.75
    it "Division by zero yields an error." $
      ".a / .b" `appliedTo` "{\"a\": 3, \"b\": 0}" `shouldThrow` zeroDivision 
  describe "Test calling prefix named functions." $ do
    it "Function's name followed by arguments calls the function." $
      "plus 1 3" `appliedTo` "{\"a\": 1, \"b\": 3}" `shouldReturn` num 4
    it "Function calls compose with getters." $
      "plus .a 1" `appliedTo` "{\"a\": 2}" `shouldReturn` num 3
    it "Function called with two getters also works." $
      "mult .[0] .[1]" `appliedTo` "[2, 5]" `shouldReturn` num 10
  describe "Test order of arithmetic operations." $ do
    it "By default operations are evaluated in the order of appearance." $
      "1 + 2 - 3 + 4" `appliedTo` "[]" `shouldReturn` num 4
    it "Operators of higher precedence bind stronger than those of lower precedence" $
      ".a * .b + .c * .d" `appliedTo` "{\"a\": 2, \"b\": 3, \"c\": 5, \"d\": 7}" `shouldReturn` num 41
    it "Parentheses can modify operation precedence." $
      "(1 + 2) * (3 + 4)" `appliedTo` "{}" `shouldReturn` num 21
    it "Function call binds more strongly than operators." $
      "mult (.[0]) .[1] + mult (.[2]) .[3]" `appliedTo` "[2, 3, 4, 5]" `shouldReturn` num 26
    it "Function call can be an operator's argument." $
      "plus .a .b * minus (.a) .b" `appliedTo` "{\"a\": 5, \"b\": 2}" `shouldReturn` num 21
    it "Parentheses bind more strongly than anything else." $
      "mult (.a + .b) 3" `appliedTo` "{\"a\": 1, \"b\": 3}" `shouldReturn` num 12
    it "If statements bind the least strongly." $
      "if .a < .b then .a else .b + 1" `appliedTo` "{\"a\": 9, \"b\": 5}" `shouldReturn` num 6
    it "Parentheses override default precedence." $
      "(if .a > .b then .a else .b) - (if .a < .b then .b else .a)"
      `appliedTo` "{\"a\": 3, \"b\": 7}" `shouldReturn` num 0
  describe "Test map function." $ do
    it "Map alters every element of an array as if it was a standalone JSON." $
      "map (id + 1)" `appliedTo` "[1, 2, 3]" `shouldReturn` array [num 2, num 3, num 4]
    it "Parts of an element can also be accessed." $ do
      "map (.a * .b)" `appliedTo` "[{\"a\": 3, \"b\": 2}, {\"a\": 5, \"b\": 3}]" `shouldReturn` array [num 6, num 15]
  describe "Test comparison functions." $ do
    it "Any two expressions can be compare for equality." $
      ".a = .b" `appliedTo` "{\"a\": 1, \"b\": null}" `shouldReturn` bool False
    it "Two the same values compare as equal" $
      "(.a + .b) = .c" `appliedTo` "{\"a\": 1, \"b\": 2, \"c\": 3}" `shouldReturn` bool True
    it "Equality does not fail on type mismatch (just returns false)." $
      ".a = 12.34" `appliedTo` "{\"a\": \"some string\"}" `shouldReturn` bool False
    it "Order of array elements matters for equality." $
      ".a = [1, 2, 3]" `appliedTo` "{\"a\": [3, 2, 1]}" `shouldReturn` bool False
    it "Order of object properties DOES NOT matter for equality." $
      "id = {\"a\": 1, \"b\": 2, \"c\": 3}" `appliedTo` "{\"c\": 3, \"b\": 2, \"a\": 1}" `shouldReturn` bool True
    it "Test less-than on numbers." $
      ".a < .b" `appliedTo` "{\"a\": 1, \"b\": 3}" `shouldReturn` bool True
    it "Test less-than-or-equal on strings" $
      ".a <= .b" `appliedTo` "{\"a\": \"aaa\", \"b\": \"ccc\"}" `shouldReturn` bool True
    it "Test comparison on array filtering." $
      "filter (id > 3)" `appliedTo` "[1, 2, 3, 4]" `shouldReturn` array [num 4]
  describe "Test null handling." $ do
    it "Question mark operator behaves like composition for non-nulls." $
      ".a ? (id + 2)" `appliedTo` "{\"a\": 0}" `shouldReturn` num 2
    it "Question mark operator maps null to null." $
      ".a ? (id + 2)" `appliedTo` "{}" `shouldReturn` null
    it "Question mark operator does not guard against type errors." $
      ".a ? (id + 2)" `appliedTo` "{\"a\": []}" `shouldThrow` notANumber (array [])
    it "Try function catches any error and returns a null." $
      "try (.a + .b)" `appliedTo` "{}" `shouldReturn` null
  describe "Test size function." $ do
    it "Size returns the size of an array." $
      "size .a" `appliedTo` "{\"a\": []}" `shouldReturn` num 0
    it "Likewise size returns the size of an object" $
      "filter (size id > 0)" `appliedTo` "[{}, {}, {\"a\": 1}]" `shouldReturn` array [obj [("a", num 1)]]
    it "Size also returns length of a string." $
      "size .[0]" `appliedTo` "[\"JSON\"]" `shouldReturn` num 4
  describe "Test union of objects." $ do
    it "Union with empty object is an identity." $
      "union id {}" `appliedTo` "{\"a\": 12}" `shouldReturn` obj [("a", num 12)]
    it "Union adds properties of the right object to the left one." $
      "union {\"a\": true} id" `appliedTo` "{\"b\": false}" `shouldReturn` obj [("a", bool True), ("b", bool False)]
  describe "Test if-then-else branching structure." $ do
    it "Non-boolean condition causes an error." $
      "if .a then .b else .c" `appliedTo` "{\"a\": 1}" `shouldThrow` notABoolean (num 1)
    it "If condition evaluates to true, the first sub-expression is returned." $
      "if .a then .b else .c" `appliedTo` "{\"a\": true, \"b\": 1}" `shouldReturn` num 1
    it "If condition evaluates to false, the second sub-expression is returned." $
      "if .a then .b else .c" `appliedTo` "{\"a\": false, \"c\": 2}" `shouldReturn` num 2
    it "Operators and functions can be used at will inside conditionals." $
      "if size (keys .a) > 0 then .b + 1 else .b - 1" `appliedTo` "{\"a\": {}, \"b\": 3}" `shouldReturn` num 2
    it "If after a composition operator parses well." $
      ".abc | if .a > 0 then .b else .c" `appliedTo` "{\"abc\": {\"a\": 1, \"b\": 2, \"c\": 3}}" `shouldReturn` num 2
  describe "Test array slicing." $ do
    it "When only a colon is present, whole list is taken." $
      ".[:]" `appliedTo` "[0, 1, 2, 3]" `shouldReturn` array [num 0, num 1, num 2, num 3]
    it "Number before colon is the first index to take." $
      ".[1:]" `appliedTo` "[0, 1, 2, 3]" `shouldReturn` array [num 1, num 2, num 3]
    it "Number following the colon is the first index to drop." $
      ".[:2]" `appliedTo` "[0, 1, 2, 3]" `shouldReturn` array [num 0, num 1]
    it "Both first and last index may be specified." $
      ".[1:3]" `appliedTo` "[0, 1, 2, 3]" `shouldReturn` array [num 1, num 2]
    it "After second colon, step may be specified" $
      ".[::2]" `appliedTo` "[0, 1, 2, 3, 4]" `shouldReturn` array [num 0, num 2, num 4]
    it "When first and step are specified, go to the end of the array." $
      ".[2::3]" `appliedTo` "[0, 1, 2, 3, 4, 5]" `shouldReturn` array [num 2, num 5]
    it "When last and step are specified, start from the beginning." $
      ".[:3:2]" `appliedTo` "[0, 1, 2, 3, 4, 5]" `shouldReturn` array [num 0, num 2]
    it "When all three elements are specified, take the slice with step." $
      ".[1:4:2]" `appliedTo` "[0, 1, 2, 3, 4, 5]" `shouldReturn` array [num 1, num 3]
    it "Negative indices are counted from the end of the array." $
      ".[-3:-1]" `appliedTo` "[0, 1, 2, 3, 4, 5]" `shouldReturn` array [num 3, num 4]
    it "When step is negative, the array should be traversed backwards." $ do
      ".[::-1]" `appliedTo` "[0, 1, 2]" `shouldReturn` array [num 2, num 1, num 0]
    it "A convoluted example." $
      ".[2:-3:-2]" `appliedTo` "[0, 1, 2, 3, 4, 5, 6, 7]" `shouldReturn` array [num 4, num 2]
      
appliedTo :: Text -> Text -> IO JsonAst
appliedTo exprTxt jsonTxt = runToIO $ do
  jsonTokens <- liftTrace $ parse tokenize "test JSON" jsonTxt
  json <- runTokenParser (fix JsonParser.tokJSON) jsonTokens
  exprTokens <- liftTrace $ parse tokenize "text expression" exprTxt
  expr <- runTokenParser langExpr exprTokens
  liftTrace $ eval expr json
  where
  langExpr = do
    e <- tokExpr
    eof
    return e

instance (Monad m, JSON a) => JSON (ExceptTraceT m a) where
  str = return . str
  num = return . num
  bool = return . bool
  null = return null
  obj kvs = obj <$> foldM retKVpair [] kvs
  array js = array <$> sequence js

retKVpair :: Monad m => [(a, b)] -> (a, m b) -> m [(a, b)]
retKVpair kvs (k, mv) = do
  v <- mv
  return $ (k, v) : kvs

notAnObject :: JsonAst -> Selector [SomeException]
notAnObject expected [e] = case fromException e of
  Just (NotAnObject actual) -> expected == actual
  Just _ -> False
  Nothing -> False
notAnObject _ _ = False

notAnArray :: JsonAst -> Selector [SomeException]
notAnArray expected [e] = case fromException e of
  Just (NotAnArray actual) -> expected == actual
  Just _ -> False
  Nothing -> False
notAnArray _ _ = False

notANumber :: JsonAst -> Selector [SomeException]
notANumber expected [e] = case fromException e of
  Just (NotANumber json) -> expected == json
  _ -> False
notANumber _ _ = False

notABoolean :: JsonAst -> Selector [SomeException]
notABoolean expected [e] = case fromException e of
  Just (NotABoolean json) -> expected == json
  _ -> False
notABoolean _ _ = False

zeroDivision :: Selector [SomeException]
zeroDivision es = case fromException $ last es of
  Just ZeroDivision -> True
  _ -> False

parseError :: Selector [SomeException]
parseError = any isParseError
  where
  isParseError e = case fromException e of
                     Just (_ :: TokParseError Token) -> True
                     _ -> False
