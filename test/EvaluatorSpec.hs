module EvaluatorSpec where

import Control.Monad (forM_, when)
import Control.Monad.State (evalStateT)
import qualified Data.Map as Map
import qualified Evaluator as E
import qualified Lexer as T
import qualified Object as O
import qualified Parser as P
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Text.Printf (printf)

spec :: Spec
spec = do
    describe "EvalIntegerExpression" testEvalIntegerExpression
    describe "EvalBooleanExpression" testEvalBooleanExpression
    describe "EvalBangOperator" testEvalBangOperator
    describe "EvalIfExpression" testEvalIfElseExpressions
    describe "EvalReturnStatements" testEvalReturnStatements
    describe "EvalErrorHandling" testErrorHandling
    describe "EvalLetStatements" testLetStatements

testEval :: String -> E.ProgramOutput
testEval = (`evalStateT` Map.empty) . E.eval . fst . P.runParser . T.runLexer

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
        it "should evaluate integer expressions" $ testIntegerObject (testEval input) output

testIntegerObject :: E.ProgramOutput -> Integer -> Expectation
testIntegerObject obj int = case obj of
    Right (O.OInt v) -> v `shouldBe` int
    _ -> expectationFailure $ printf "Expected integer object, got %s" (show obj)

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
        it "should evaluate boolean expressions" $ testBooleanObject (testEval input) output

testBooleanObject :: E.ProgramOutput -> Bool -> Expectation
testBooleanObject obj bool = case obj of
    Right (O.OBool v) -> v `shouldBe` bool
    _ -> expectationFailure $ printf "Expected boolean object, got %s" (show obj)

testEvalBangOperator = do
    let inputs = ["!true", "!false", "!5", "!!true", "!!false", "!!5"]
        outputs = [False, True, False, True, False, True]

    forM_ (zip inputs outputs) $ \(input, output) -> do
        it "should evaluate bang expressions" $ testBooleanObject (testEval input) output

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
            case output of
                Just v -> testIntegerObject (testEval input) v
                _ -> testNullObject (testEval input)

testNullObject :: E.ProgramOutput -> Expectation
testNullObject obj =
    when (obj /= Right O.ONull) $
        expectationFailure $
            printf "Expected null object, got %s" (show obj)

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
        it "should evaluate return statements" $ testIntegerObject (testEval input) output

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
              ("foobar", "identifier not found: foobar")
            ]

    forM_ tests $ \(input, output) -> do
        it "should handle errors" $ do
            let evaluation = testEval input
            case evaluation of
                Left error -> error `shouldBe` output
                _ -> expectationFailure $ printf "Expected error message %s, got %s" output (show evaluation)

testLetStatements = do
    let tests =
            [ ("let a = 5; a;", 5),
              ("let a = 5 * 5; a;", 25),
              ("let a = 5; let b = a; b;", 5),
              ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
            ]

    forM_ tests $ \(input, output) -> do
        it "should evaluate let statements" $ testIntegerObject (testEval input) output