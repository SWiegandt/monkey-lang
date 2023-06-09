module Monkey.Eval.EvaluatorSpec where

import Control.Monad (forM_, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (evalStateT)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Monkey.Eval.Evaluator as E
import qualified Monkey.Eval.Object as O
import qualified Monkey.Frontend.Lexer as T
import qualified Monkey.Frontend.Parser as P
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Text.Printf (printf)

spec :: Spec
spec = do
    describe "EvalIntegerExpression" testEvalIntegerExpression
    describe "EvalBooleanExpression" testEvalBooleanExpression
    describe "EvalStringExpression" testEvalStringExpression
    describe "EvalBangOperator" testEvalBangOperator
    describe "EvalIfExpression" testEvalIfElseExpressions
    describe "EvalReturnStatements" testEvalReturnStatements
    describe "EvalErrorHandling" testErrorHandling
    describe "EvalLetStatements" testLetStatements
    describe "EvalFunctionObject" testFunctionObject
    describe "EvalFunctionApplication" testFunctionApplication
    describe "EvalFunctionClosures" testClosures
    describe "EvalBuiltinFunctions" testBuiltinFunctions
    describe "EvalArrayExpression" testArrayExpressions
    describe "EvalIndexExpression" testIndexExpressions
    describe "EvalHashExpression" testHashExpressions
    describe "EvalHashIndexExpression" testHashIndexExpressions

testEval :: String -> IO E.ProgramOutput
testEval program = do
    env <- O.EnvRef <$> newIORef (O.Env Map.empty Nothing)
    runExceptT . (`evalStateT` env) . E.eval . fst . P.runParser . T.runLexer $ program

inspect :: E.ProgramOutput -> String
inspect o = show $ O.inspect <$> o

testIntegerObject :: Integer -> E.ProgramOutput -> Expectation
testIntegerObject int obj = case obj of
    Right (O.Int v) -> v `shouldBe` int
    _ -> expectationFailure $ printf "Expected integer object, got %s" (inspect obj)

testEvalIntegerExpression = do
    let tests =
            [ ("5", 5),
              ("10", 10),
              ("-5", -5),
              ("-10", -10),
              ("5 + 5 + 5 + 5 - 10", 10),
              ("2 * 2 * 2 * 2 * 2", 32),
              ("-50 + 100 + -50", 0),
              ("5 * 2 + 10", 20),
              ("5 + 2 * 10", 25),
              ("20 + 2 * -10", 0),
              ("50 / 2 * 2 + 10", 60),
              ("2 * (5 + 10)", 30),
              ("3 * 3 * 3 + 10", 37),
              ("3 * (3 * 3) + 10", 37),
              ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate integer expressions" $ testEval input >>= testIntegerObject output

testBooleanObject :: Bool -> E.ProgramOutput -> Expectation
testBooleanObject bool obj = case obj of
    Right (O.Bool v) -> v `shouldBe` bool
    _ -> expectationFailure $ printf "Expected boolean object, got %s" (inspect obj)

testEvalBooleanExpression = do
    let tests =
            [ ("true", True),
              ("false", False),
              ("1 < 2", True),
              ("1 > 2", False),
              ("1 < 1", False),
              ("1 > 1", False),
              ("1 == 1", True),
              ("1 != 1", False),
              ("1 == 2", False),
              ("1 != 2", True),
              ("true == true", True),
              ("false == false", True),
              ("true == false", False),
              ("true != false", True),
              ("false != true", True),
              ("(1 < 2) == true", True),
              ("(1 < 2) == false", False),
              ("(1 > 2) == true", False),
              ("(1 > 2) == false", True)
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate boolean expressions" $ testEval input >>= testBooleanObject output

testStringObject :: String -> E.ProgramOutput -> Expectation
testStringObject str obj = case obj of
    Right (O.String v) -> v `shouldBe` str
    _ -> expectationFailure $ printf "Expected string object, got %s" (inspect obj)

testEvalStringExpression = do
    let tests =
            [ ("\"hello world\"", "hello world"),
              ("\"hello\" + \" \" + \"world\"", "hello world")
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate string expressions" $ testEval input >>= testStringObject output

testEvalBangOperator = do
    let inputs = ["!true", "!false", "!5", "!!true", "!!false", "!!5"]
        outputs = [False, True, False, True, False, True]

    forM_ (zip inputs outputs) $ \(input, output) -> do
        it "should evaluate bang expressions" $ testEval input >>= testBooleanObject output

testEvalIfElseExpressions = do
    let tests =
            [ ("if (true) { 10 }", Just 10),
              ("if (false) { 10 }", Nothing),
              ("if (1) { 10 }", Just 10),
              ("if (1 < 2) { 10 }", Just 10),
              ("if (1 > 2) { 10 }", Nothing),
              ("if (1 > 2) { 10 } else { 20 }", Just 20),
              ("if (1 < 2) { 10 } else { 20 }", Just 10)
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate if expressions" $ do
            result <- testEval input
            case output of
                Just v -> testIntegerObject v result
                _ -> testNullObject result

testNullObject :: E.ProgramOutput -> Expectation
testNullObject obj =
    when (obj /= Right O.Null) $
        expectationFailure $
            printf "Expected null object, got %s" (inspect obj)

testEvalReturnStatements = do
    let tests =
            [ ("return 10;", 10),
              ("return 10; 9;", 10),
              ("return 2 * 5; 9;", 10),
              ("9; return 2 * 5; 9;", 10),
                ( unlines
                    [ "if (10 > 1) {",
                      "  if (10 > 1) {",
                      "    return 10;",
                      "  }",
                      "  return 1;",
                      "}"
                    ],
                  10
                )
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate return statements" $ testEval input >>= testIntegerObject output

testErrorHandling = do
    let tests =
            [ ("5 + true;", "type mismatch: IntegerType + BooleanType"),
              ("5 + true; 5;", "type mismatch: IntegerType + BooleanType"),
              ("-true", "unknown operator: -BooleanType"),
              ("true + false;", "unknown operator: BooleanType + BooleanType"),
              ("true + false + true + false;", "unknown operator: BooleanType + BooleanType"),
              ("5; true + false; 5", "unknown operator: BooleanType + BooleanType"),
              ("if (10 > 1) { true + false; }", "unknown operator: BooleanType + BooleanType"),
                ( unlines
                    [ "if (10 > 1) {",
                      "  if (10 > 1) {",
                      "    return true + false;",
                      "  }",
                      "  return 1;",
                      "}"
                    ],
                  "unknown operator: BooleanType + BooleanType"
                ),
              ("foobar", "identifier not found: foobar"),
              ("\"hello\" - \"world\"", "unknown operator: StringType - StringType"),
              ("{\"name\": \"Monkey\"}[fn(x) { x }];", "unusable as hash key: FunctionType")
            ]

    forM_ tests $ \(input, output) -> do
        it "should handle errors" $ do
            result <- testEval input
            case result of
                Left error -> error `shouldBe` output
                _ -> expectationFailure $ printf "Expected error message %s, got %s" output (inspect result)

testLetStatements = do
    let tests =
            [ ("let a = 5; a;", 5),
              ("let a = 5 * 5; a;", 25),
              ("let a = 5; let b = a; b;", 5),
              ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate let statements" $ testEval input >>= testIntegerObject output

testFunctionObject = do
    let test = "fn(x) { x + 2 };"

    it "should evaluate function objects" $ do
        result <- testEval test

        case result of
            Right (O.Function params body _) -> do
                length params `shouldBe` 1
                show (head params) `shouldBe` "x"
                show body `shouldBe` "(x + 2)"
            _ -> expectationFailure $ printf "Expected function object, got %s" (inspect result)

testFunctionApplication = do
    let tests =
            [ ("let identity = fn(x) { x; }; identity(5);", 5),
              ("let identity = fn(x) { return x; }; identity(5);", 5),
              ("let double = fn(x) { x * 2; }; double(5);", 10),
              ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
              ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
              ("fn(x) { x; }(5)", 5)
            ]

    forM_ tests $ \(input, output) -> do
        it "can apply functions" $ testEval input >>= testIntegerObject output

testClosures = do
    let test =
            unlines
                [ "let newAdder = fn(x) {",
                  "  fn(y) { x + y };",
                  "};",
                  "let addTwo = newAdder(2);",
                  "addTwo(2);"
                ]

    it "handles closures" $ testEval test >>= testIntegerObject 4

testBuiltinFunctions = do
    let tests =
            [ ("len(\"\")", Right 0),
              ("len(\"four\")", Right 4),
              ("len(\"hello world\")", Right 11),
              ("len(1)", Left "argument to `len` not supported, got IntegerType"),
              ("len(\"one\", \"two\")", Left "wrong number of arguments. got=2, want=1")
            ]

    forM_ tests $ \(input, output) -> do
        it "can apply builtins" $ do
            result <- testEval input
            case output of
                Right n -> testIntegerObject n result
                Left s -> case result of
                    Left err -> err `shouldBe` s
                    Right _ -> expectationFailure $ printf "Expected error, got %s" (inspect result)

testArrayExpressions = do
    let input = "[1, 2 * 2, 3 + 3]"

    it "evaluates arrays" $ do
        result <- testEval input
        case result of
            Right (O.Array elements) -> do
                length elements `shouldBe` 3
                testIntegerObject 1 (Right $ head elements)
                testIntegerObject 4 (Right $ elements !! 1)
                testIntegerObject 6 (Right $ elements !! 2)
            Right o -> expectationFailure $ printf "Expected array object, got %s" (O.inspect o)
            Left err -> expectationFailure $ printf "Got error %s" err

testIndexExpressions = do
    let tests =
            [ ("[1, 2, 3][0]", Just 1),
              ("[1, 2, 3][1]", Just 2),
              ("[1, 2, 3][2]", Just 3),
              ("let i = 0; [1][i];", Just 1),
              ("[1, 2, 3][1 + 1];", Just 3),
              ("let myArray = [1, 2, 3]; myArray[2];", Just 3),
              ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Just 6),
              ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Just 2),
              ("[1, 2, 3][3]", Nothing),
              ("[1, 2, 3][-1]", Nothing)
            ]

    forM_ tests $ \(input, output) -> it "evaluates index expression" $ do
        result <- testEval input
        case output of
            Just n -> testIntegerObject n result
            Nothing -> testNullObject result

testHashExpressions = do
    let input = "let two = \"two\"; {\"one\": 10 - 9, two: 1 + 1, \"thr\" + \"ee\": 6 / 2, 4: 4, true: 5, false: 6}"

    it "evaluates hash expression" $ do
        result <- testEval input
        case result of
            Right (O.Hash map) -> do
                Map.size map `shouldBe` 6
                map Map.! O.String "one" `shouldBe` O.Int 1
            Right o -> expectationFailure $ printf "Expected hash object, got %s" (O.inspect o)
            Left err -> expectationFailure $ printf "Got error %s" err

testHashIndexExpressions = do
    let tests =
            [ ("{\"foo\": 5}[\"foo\"]", Just 5),
              ("{\"foo\": 5}[\"bar\"]", Nothing),
              ("let key = \"foo\"; {\"foo\": 5}[key]", Just 5),
              ("{}[\"foo\"]", Nothing),
              ("{5: 5}[5]", Just 5),
              ("{true: 5}[true]", Just 5),
              ("{false: 5}[false]", Just 5)
            ]

    forM_ tests $ \(input, output) -> do
        it "evaluates hash index expressions" $ do
            result <- testEval input
            case output of
                Just n -> result `shouldBe` Right (O.Int n)
                Nothing -> result `shouldBe` Right O.Null
