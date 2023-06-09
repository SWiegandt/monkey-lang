{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Monkey.Frontend.ParserSpec (spec) where

import Control.Monad (forM_, void, when)
import Control.Monad.State.Strict (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import qualified Data.Map.Strict as Map
import qualified Monkey.AST.Nodes as N
import qualified Monkey.AST.Tokens as T
import Monkey.Frontend.Lexer (runLexer)
import Monkey.Frontend.Parser (runParser)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Text.Printf (printf)

spec :: Spec
spec = do
    describe "LetStmt" testLetStatements
    describe "ReturnStmt" testReturnStatements
    describe "Show" testShow
    describe "IdentifierExpr" testIdentifierExpression
    describe "IntegerExpr" testIntegerLiteralExpression
    describe "BooleanExpr" testBooleanExpression
    describe "StringExpr" testStringLiteralExpression
    describe "prefixParse" testPrefixParse
    describe "infixParse" $ do
        testInfixParse
        testOperatorPrecedence
    describe "IfExpression" testIfExpression
    describe "IfElseExpression" testIfElseExpression
    describe "FunctionExpr" testFunctionExpression
    describe "Function parameters" testFunctionParameterParsing
    describe "CallExpression" testCallExpression
    describe "ArrayExpression" testArrayExpression
    describe "IndexExpression" testIndexExpression
    describe "HashExpression" testHashExpression

checkErrorLog log = it "reports no errors" $ log `shouldSatisfy` null

testLetStatements = do
    let inputs = ["let x = 5;", "let y = true;", "let foobar = y;"]
        outputs = [("x", IntExpectation 5), ("y", BooleanExpectation True), ("foobar", StringExpectation "y")]

    forM_ (zip inputs outputs) $ \(input, output) -> do
        let (N.Program stmts@(statement@(N.LetStmt _ _ value) : _), log) = runParser . runLexer $ input
        checkErrorLog log
        it "should have one statement" $ length stmts `shouldBe` 1
        testLetStatement statement (fst output)
        it "has correct value" $ testLiteralExpression value (snd output)

testLetStatement statement@(N.LetStmt _ (N.Identifier token name) _) expected = do
    it "should have tokenLiteral 'let'" $ N.tokenLiteral statement `shouldBe` "let"

    it "should be a let statement" $ case statement of
        N.LetStmt {} -> return ()
        _ -> expectationFailure $ printf "Expected LetStmt, got %s" (show statement)

    it (printf "should have correct name %s" name) $ do
        name `shouldBe` expected
        T.literal token `shouldBe` expected

testReturnStatements = do
    let inputs = ["return 5;", "return true;", "return foobar;"]
        outputs = [IntExpectation 5, BooleanExpectation True, StringExpectation "foobar"]

    forM_ (zip inputs outputs) $ \(input, output) -> do
        let (N.Program stmts@(statement@(N.ReturnStmt _ value) : _), log) = runParser . runLexer $ input
        checkErrorLog log
        it "should have one statement" $ length stmts `shouldBe` 1
        testReturnStatement statement
        it "has correct value" $ testLiteralExpression value output

testReturnStatement statement = do
    it "should have tokenLiteral 'return'" $ N.tokenLiteral statement `shouldBe` "return"

    it "should be a return statement" $ case statement of
        N.ReturnStmt {} -> return ()
        _ -> expectationFailure $ printf "Expected ReturnStmt, got %s" (show statement)

testShow = do
    let program =
            N.Program
                [ N.LetStmt
                    (T.Token T.Let "let")
                    (N.Identifier (T.Token T.Ident "myVar") "myVar")
                    (N.IdentifierExpression (N.Identifier (T.Token T.Ident "anotherVar") "anotherVar"))
                ]

    it "stringifies statements" $ do
        show program `shouldBe` "let myVar = anotherVar;"

testIdentifierExpression = do
    let input = "foobar;"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ length stmts `shouldBe` 1

    it "should parse identity expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IdentifierExpression i@(N.Identifier t v) -> do
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
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ length stmts `shouldBe` 1

    it "should parse integer literal expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            e@(N.IntegerExpression t v) -> do
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
        let (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

        checkErrorLog log

        it "should have the correct number of expressions" $ length stmts `shouldBe` 1

        it "should parse boolean expressions" $ case statement of
            N.ExpressionStmt _ expr -> case expr of
                e@(N.BooleanExpression t v) -> do
                    when (v /= output) $
                        expectationFailure $
                            printf "Expected %s value, got %s" (show output) (show v)
                _ -> expectationFailure $ printf "Expected BooleanExpr, got %s" (show expr)
            _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testStringLiteralExpression = do
    let input = "\"hello world\""
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ length stmts `shouldBe` 1

    it "should parse string literal expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            e@(N.StringExpression t v) -> do
                when (v /= "hello world") $
                    expectationFailure $
                        printf "Expected 'hello world' value, got %s" v

                when (N.tokenLiteral e /= "hello world") $
                    expectationFailure $
                        printf "Expected 'hello world' literal, got %s" (N.tokenLiteral e)
            _ -> expectationFailure $ printf "Expected StringLiteralExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testPrefixParse = do
    let inputs = ["!5;", "-15;"]
        expecteds = [("!", 5), ("-", 15)]

    forM_ (zip inputs expecteds) $ \(input, expected) -> do
        let (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

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
        let (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

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
    expr@(N.IntegerExpression _ i) -> do
        when (i /= expected) $ hoistMaybe Nothing
        when (N.tokenLiteral expr /= show expected) $ hoistMaybe Nothing
    _ -> lift . expectationFailure $ printf "Expected integer literal, got %s" $ show rhs

testIdentifier :: N.Expression -> String -> Expectation
testIdentifier expr value = case expr of
    N.IdentifierExpression i@(N.Identifier _ v) -> do
        when (v /= value) $ expectationFailure $ printf "Expected value %s, got %s" value v
        when (N.tokenLiteral i /= value) $
            expectationFailure $
                printf "Expected literal %s, got %s" value (N.tokenLiteral i)
    _ -> expectationFailure $ printf "Expected IdentifierExpr, got %s" (show expr)

testBooleanLiteral :: N.Expression -> Bool -> Expectation
testBooleanLiteral expr value = case expr of
    b@(N.BooleanExpression _ v) -> do
        when (v /= value) $ printf "Expected boolean value %s, got %s" (show value) (show v)
        when (N.tokenLiteral b /= show value) $ printf "Expected token literal %s, got %s" (show value) (N.tokenLiteral b)
    _ -> expectationFailure $ printf "Expected BooleanExpr, got %s" (show expr)

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
testInfixExpression expr _ _ _ = expectationFailure $ printf "Expected InfixExpression, got %s" (show expr)

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
              ["!(true == true)", "(!(true == true))"],
              ["a + add(b * c) + d", "((a + add((b * c))) + d)"],
              ["add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"],
              ["add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"],
              ["add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"]
            ]

    forM_ inputs $ \[input, output] -> do
        let (program, log) = runParser . runLexer $ input
        checkErrorLog log
        it "should parse operator precedence correctly" $ show program `shouldBe` output

testIfExpression = do
    let input = "if (x < y) { x }"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

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
                    N.ExpressionStmt _ (N.IdentifierExpression (N.Identifier _ s)) -> s `shouldBe` "x"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head cons))
            _ -> expectationFailure $ printf "Expected IfExpression with no else, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testIfElseExpression = do
    let input = "if (x < y) { x } else { y }"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

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
                    N.ExpressionStmt _ (N.IdentifierExpression (N.Identifier _ s)) -> s `shouldBe` "x"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head cons))

                case head alt of
                    N.ExpressionStmt _ (N.IdentifierExpression (N.Identifier _ s)) -> s `shouldBe` "y"
                    _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show (head alt))
            _ -> expectationFailure $ printf "Expected IfExpression with else, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testFunctionExpression = do
    let input = "fn(x, y) { x + y; }"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statement" $ length stmts `shouldBe` 1

    it "should parse function expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.FunctionExpression _ params body -> do
                length params `shouldBe` 2
                testLiteralExpression (N.IdentifierExpression $ head params) (StringExpectation "x")
                testLiteralExpression (N.IdentifierExpression $ params !! 1) (StringExpectation "y")

                case body of
                    N.Block [N.ExpressionStmt _ stmt] ->
                        testInfixExpression stmt (StringExpectation "x") "+" (StringExpectation "y")
                    _ -> expectationFailure $ printf "Expected body with one statement, got %s" (show body)
            _ -> expectationFailure $ printf "Expected FunctionExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testFunctionParameterParsing = do
    let inputs = ["fn() {};", "fn(x) {};", "fn(x, y, z) {};"]
        outputs = [[], ["x"], ["x", "y", "z"]]

    forM_ (zip inputs outputs) $ \(input, output) -> do
        let (N.Program [N.ExpressionStmt _ (N.FunctionExpression _ params _)], log) = runParser . runLexer $ input

        checkErrorLog log

        it "should parse params correctly" $ do
            length params `shouldBe` length output

            forM_ (zip params output) $ \(param, expected) ->
                testLiteralExpression (N.IdentifierExpression param) (StringExpectation expected)

testCallExpression = do
    let input = "add(1, 2 * 3, 4 + 5);"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statement" $ length stmts `shouldBe` 1

    it "should parse call expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.CallExpression _ fn args -> do
                testIdentifier fn "add"
                length args `shouldBe` 3
                testLiteralExpression (head args) (IntExpectation 1)
                testInfixExpression (args !! 1) (IntExpectation 2) "*" (IntExpectation 3)
                testInfixExpression (args !! 2) (IntExpectation 4) "+" (IntExpectation 5)
            _ -> expectationFailure $ printf "Expected CallExpression, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testArrayExpression = do
    let input = "[1, 2 * 2, 3 + 3]"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statement" $ length stmts `shouldBe` 1

    it "should parse array expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.ArrayExpression _ elements -> do
                length elements `shouldBe` 3
                void . runMaybeT $ testIntegerLiteral (head elements) 1
                testInfixExpression (elements !! 1) (IntExpectation 2) "*" (IntExpectation 2)
                testInfixExpression (elements !! 2) (IntExpectation 3) "+" (IntExpectation 3)
            _ -> expectationFailure $ printf "Expected ArrayExpression, got %s" (show statement)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testIndexExpression = do
    let input = "myArray[1 + 1]"
        (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should contain one statement" $ length stmts `shouldBe` 1

    it "should parse index expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IndexExpression _ lhs rhs -> do
                testIdentifier lhs "myArray"
                testInfixExpression rhs (IntExpectation 1) "+" (IntExpectation 1)
            _ -> expectationFailure $ printf "Expected ArrayExpression, got %s" (show statement)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testHashExpression = do
    let tests =
            [   ( "{\"one\": 1, \"two\": 2, \"three\": 3}",
                  Map.fromList
                    [ ("one", void . runMaybeT . (`testIntegerLiteral` 1)),
                      ("two", void . runMaybeT . (`testIntegerLiteral` 2)),
                      ("three", void . runMaybeT . (`testIntegerLiteral` 3))
                    ]
                ),
              ("{}", Map.empty),
                ( "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
                  Map.fromList
                    [ ("one", \e -> testInfixExpression e (IntExpectation 0) "+" (IntExpectation 1)),
                      ("two", \e -> testInfixExpression e (IntExpectation 10) "-" (IntExpectation 8)),
                      ("three", \e -> testInfixExpression e (IntExpectation 15) "/" (IntExpectation 5))
                    ]
                )
            ]

    forM_ tests $ \(input, testFns) -> do
        let (N.Program stmts@(statement : _), log) = runParser . runLexer $ input

        checkErrorLog log

        it "should parse hash expression" $ case statement of
            N.ExpressionStmt _ expr -> case expr of
                N.HashExpression _ map -> do
                    Map.size map `shouldBe` length testFns
                    forM_ (Map.keys map) $ \key -> (testFns Map.! show key) (map Map.! key)
                _ -> expectationFailure $ printf "Expected HashExpression, got %s" (show statement)
            _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)
