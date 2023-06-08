{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ParserSpec (spec) where

import Control.Monad (forM_, zipWithM_)
import Control.Monad.State (evalState)
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

    it "should have the correct number of expressions" $ do
        length stmts `shouldBe` 1

    it "should parse identity expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            N.IdentifierExpr i@(N.Identifier t v) -> do
                case v of
                    "foobar" -> return ()
                    _ -> expectationFailure $ printf "Expected foobar value, got %s" v

                case N.tokenLiteral i of
                    "foobar" -> return ()
                    _ -> expectationFailure $ printf "Expected foobar literal, got %s" (N.tokenLiteral i)
            _ -> expectationFailure $ printf "Expected IdentifierExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)

testIntegerLiteralExpression = do
    let input = "5;"
        (Program stmts@(statement : _), log) = runParser . runLexer $ input

    checkErrorLog log

    it "should have the correct number of expressions" $ do
        length stmts `shouldBe` 1

    it "should parse integer literal expressions" $ case statement of
        N.ExpressionStmt _ expr -> case expr of
            e@(N.IntegerLiteralExpr t v) -> do
                case v of
                    5 -> return ()
                    _ -> expectationFailure $ printf "Expected 5 value, got %s" v

                case N.tokenLiteral e of
                    "5" -> return ()
                    _ -> expectationFailure $ printf "Expected '5' literal, got %s" (N.tokenLiteral e)
            _ -> expectationFailure $ printf "Expected IntegerLiteralExpr, got %s" (show expr)
        _ -> expectationFailure $ printf "Expected ExpressionStmt, got %s" (show statement)
