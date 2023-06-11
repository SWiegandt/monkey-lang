{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ParserSpec (spec) where

import Control.Monad (forM_, void, when, zipWithM_)
import Control.Monad.State (MonadTrans (lift), evalState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Debug.Trace (trace, traceM)
import Lexer (runLexer)
import qualified Nodes as N
import Parser (Program (Program), runParser)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldSatisfy)
import Text.Printf (printf)
import qualified Tokens as T

spec :: Spec
spec = do
    describe "LetStmt" testLetStatements
    describe "ReturnStmt" testReturnStatements
    describe "Show" testShow
    describe "IdentifierExpr" testIdentifierExpression
    describe "IntegerLiteralExpr" testIntegerLiteralExpression
    describe "BooleanExpr" testBooleanExpression
    describe "prefixParse" testPrefixParse
    describe "infixParse" $ do
        testInfixParse
        testOperatorPrecedence
    describe "IfExpression" testIfExpression
    describe "IfElseExpression" testIfElseExpression

checkErrorLog log = it "reports no errors" $ log `shouldSatisfy` null

testLetStatements = do
    let input =
            unlines
                [ "let x = 5;",
                  "let y = 10;",
                  "let foobar = 838383;"
                ]
        (Program statements, log) = runParser . runLexer $ input

    checkErrorLog log
    it "parses let statements" $ length statements `shouldBe` 3
    zipWithM_ testLetStatement statements ["x", "y", "foobar"]

testLetStatement statement name = do
    it "should have tokenLiteral 'let'" $ N.tokenLiteral statement `shouldBe` "let"

    it "should be a let statement" $ case statement of
        N.LetStmt {} -> return ()
        _ -> expectationFailure $ printf "Expected LetStmt, got %s" (show statement)

    it (printf "should have correct name %s" name) $ do
        N.identValue (N.letName statement) `shouldBe` name
        N.tokenLiteral (N.letName statement) `shouldBe` name

testReturnStatements = do
    let input =
            unlines
                [ "return 5;",
                  "return 10;",
                  "return 993322;"
                ]
        (Program statements, log) = runParser . runLexer $ input

    checkErrorLog log
    it "parses return statements" $ length statements `shouldBe` 3
    mapM_ testReturnStatement statements

testReturnStatement statement = do
    it "should have tokenLiteral 'return'" $ N.tokenLiteral statement `shouldBe` "return"

    it "should be a return statement" $ case statement of
        N.ReturnStmt {} -> return ()
        _ -> expectationFailure $ printf "Expected ReturnStmt, got %s" (show statement)

testShow = do
    let program =
            Program
                [ N.LetStmt
                    { N.letToken = T.Token T.Let "let",
                      N.letName = N.Identifier (T.Token T.Ident "myVar") "myVar",
                      N.letValue = N.IdentifierExpr (N.Identifier (T.Token T.Ident "anotherVar") "anotherVar")
                    }
                ]

    it "stringifies statements" $ do
        show program `shouldBe` "let myVar = anotherVar;"

testIdentifierExpression = do
    let input = "foobar;"
        (Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ length stmts `shouldBe` 1

    it "should parse identity expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IdentifierExpr i@(N.Identifier t v) -> do
                when (v /= "foobar") $
                    expectationFailure $
                        printf "Expected foobar literal, got %s" v

                when (N.tokenLiteral i /= "foobar") $
                    expectationFailure $
                        printf "Expected 'foobar' literal, got %s" (N.tokenLiteral i)
            _ -> expectationFailure $ printf "Expected IdentifierExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testIntegerLiteralExpression = do
    let input = "5;"
        (Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ length stmts `shouldBe` 1

    it "should parse integer literal expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            e@(N.IntegerLiteralExpr t v) -> do
                when (v /= 5) $
                    expectationFailure $
                        printf "Expected 5 value, got %s" v

                when (N.tokenLiteral e /= "5") $
                    expectationFailure $
                        printf "Expected '5' literal, got %s" (N.tokenLiteral e)
            _ -> expectationFailure $ printf "Expected IntegerLiteralExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testBooleanExpression = do
    let inputs = [("true;", True), ("false;", False)]

    forM_ inputs $ \(input, output) -> do
        let (Program stmts@(statement : _), log) = runParser . runLexer $ input

        checkErrorLog log

        it "should have the correct number of expressions" $ length stmts `shouldBe` 1

        it "should parse boolean expressions" $ case statement of
            N.ExpressionStmt _ expr -> case expr of
                e@(N.BooleanExpr t v) -> do
                    when (v /= output) $
                        expectationFailure $
                            printf "Expected %s value, got %s" (show output) (show v)
                _ -> expectationFailure $ printf "Expected BooleanExpr, got %s" (show expr)
            _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testPrefixParse = do
    let inputs = ["!5;", "-15;"]
        expecteds = [("!", 5), ("-", 15)]

    forM_ (zip inputs expecteds) $ \(input, expected) -> do
        let (Program stmts@(statement : _), log) = runParser . runLexer $ input

        checkErrorLog log

        it "should contain one statement" $ length stmts `shouldBe` 1

        it "should be an expression statement" $ case statement of
            N.ExpressionStmt _ expr -> case expr of
                N.PrefixExpression t op rhs -> do
                    when (op /= fst expected) $
                        expectationFailure $
                            printf "Expected operator %s, got %s" (fst expected) op

                    void . runMaybeT $ testIntegerLiteral rhs (snd expected)
                _ -> expectationFailure $ printf "Expected prefix expression, got %s" $ show expr
            _ -> expectationFailure $ printf "Expected expression statement, got %s" $ show statement

testInfixParse = do
    let inputs = ["5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 > 5;", "5 < 5;", "5 == 5;", "5 != 5;"]
        expecteds = [(5, "+", 5), (5, "-", 5), (5, "*", 5), (5, "/", 5), (5, ">", 5), (5, "<", 5), (5, "==", 5), (5, "!=", 5)]

    forM_ (zip inputs expecteds) $ \(input, (el, eo, er)) -> do
        let (Program stmts@(statement : _), log) = runParser . runLexer $ input

        checkErrorLog log

        it "should contain one statement" $ length stmts `shouldBe` 1

        it "should be an expression statement" $ case statement of
            N.ExpressionStmt _ expr -> case expr of
                N.InfixExpression t lhs op rhs -> do
                    void . runMaybeT $ testIntegerLiteral lhs el

                    when (op /= eo) $
                        expectationFailure $
                            printf "Expected operator %s, got %s" eo op

                    void . runMaybeT $ testIntegerLiteral rhs er
                _ -> expectationFailure $ printf "Expected infix expression, got %s" $ show expr
            _ -> expectationFailure $ printf "Expected expression statement, got %s" $ show statement

testIntegerLiteral :: N.Expression -> Integer -> MaybeT IO ()
testIntegerLiteral rhs expected = case rhs of
    expr@(N.IntegerLiteralExpr _ i) -> do
        when (i /= expected) $ hoistMaybe Nothing
        when (N.tokenLiteral expr /= show expected) $ hoistMaybe Nothing
    _ -> lift . expectationFailure $ printf "Expected integer literal, got %s" $ show rhs

testIdentifier :: N.Expression -> String -> Expectation
testIdentifier expr value = case expr of
    N.IdentifierExpr i@(N.Identifier _ v) -> do
        when (v /= value) $ expectationFailure $ printf "Expected value %s, got %s" value v
        when (N.tokenLiteral i /= value) $
            expectationFailure $
                printf "Expected literal %s, got %s" value (N.tokenLiteral i)
    _ -> expectationFailure $ printf "Expected IdentifierExpr, got %s" (show expr)

testBooleanLiteral :: N.Expression -> Bool -> Expectation
testBooleanLiteral expr value = case expr of
    b@(N.BooleanExpr _ v) -> do
        when (v /= value) $ printf "Expected boolean value %s, got %s" (show value) (show v)
        when (N.tokenLiteral b /= show value) $ printf "Expected token literal %s, got %s" (show value) (N.tokenLiteral b)

data ExpressionExpectation
    = IntExpectation Integer
    | StringExpectation String
    | BooleanExpectation Bool

testLiteralExpression :: N.Expression -> ExpressionExpectation -> Expectation
testLiteralExpression expr (IntExpectation i) = void . runMaybeT $ testIntegerLiteral expr i
testLiteralExpression expr (StringExpectation s) = testIdentifier expr s
testLiteralExpression expr (BooleanExpectation b) = testBooleanLiteral expr b

testInfixExpression :: N.Expression -> ExpressionExpectation -> String -> ExpressionExpectation -> Expectation
testInfixExpression (N.InfixExpression _ lhs op rhs) lhse ope rhse = do
    testLiteralExpression lhs lhse
    when (op /= ope) $ printf "Expected operator %s, got %s" ope op
    testLiteralExpression rhs rhse

testOperatorPrecedence = do
    let inputs =
            [ ["-a * b", "((-a) * b)"],
              ["!-a", "(!(-a))"],
              ["a + b + c", "((a + b) + c)"],
              ["a + b - c", "((a + b) - c)"],
              ["a * b * c", "((a * b) * c)"],
              ["a * b / c", "((a * b) / c)"],
              ["a + b / c", "(a + (b / c))"],
              ["a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"],
              ["3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"],
              ["5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"],
              ["5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"],
              ["3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"],
              ["true", "true"],
              ["false", "false"],
              ["3 > 5 == false", "((3 > 5) == false)"],
              ["3 < 5 == true", "((3 < 5) == true)"],
              ["1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"],
              ["(5 + 5) * 2", "((5 + 5) * 2)"],
              ["2 / (5 + 5)", "(2 / (5 + 5))"],
              ["(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"],
              ["-(5 + 5)", "(-(5 + 5))"],
              ["!(true == true)", "(!(true == true))"]
            ]

    forM_ inputs $ \[input, output] -> do
        let (program, log) = runParser . runLexer $ input
        checkErrorLog log
        it "should parse operator precedence correctly" $ show program `shouldBe` output

testIfExpression = do
    let input = "if (x < y) { x }"
        (Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statment" $ length stmts `shouldBe` 1

    it "should parse if expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IfExpression _ cond (N.Block cons) Nothing -> do
                case cond of
                    N.InfixExpression _ lhs op rhs -> (show lhs, op, show rhs) `shouldBe` ("x", "<", "y")
                    _ -> expectationFailure $ printf "Expected InfixExpression, got %s" (show cond)

                length cons `shouldBe` 1

                case head cons of
                    N.ExpressionStmt _ (N.IdentifierExpr (N.Identifier _ s)) -> s `shouldBe` "x"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head cons))
            _ -> expectationFailure $ printf "Expected IfExpression with no else, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testIfElseExpression = do
    let input = "if (x < y) { x } else { y }"
        (Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statment" $ length stmts `shouldBe` 1

    it "should parse if-else expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IfExpression _ cond (N.Block cons) (Just (N.Block alt)) -> do
                case cond of
                    N.InfixExpression _ lhs op rhs -> (show lhs, op, show rhs) `shouldBe` ("x", "<", "y")
                    _ -> expectationFailure $ printf "Expected InfixExpression, got %s" (show cond)

                length cons `shouldBe` 1

                case head cons of
                    N.ExpressionStmt _ (N.IdentifierExpr (N.Identifier _ s)) -> s `shouldBe` "x"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head cons))

                case head alt of
                    N.ExpressionStmt _ (N.IdentifierExpr (N.Identifier _ s)) -> s `shouldBe` "y"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head alt))
            _ -> expectationFailure $ printf "Expected IfExpression with else, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)
