module Monkey.Frontend.LexerSpec (spec) where

import qualified Monkey.AST.Tokens as T
import Monkey.Frontend.Lexer (runLexer)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "Lexer.runLexer" $
        it "lexes correctly" testRunLexer

testRunLexer :: Expectation
testRunLexer =
    runLexer input `shouldBe` expected
    where
        input =
            unlines
                [ "let five = 5;",
                  "let ten = 10;",
                  "",
                  "let add = fn(x, y) {",
                  "  x + y;",
                  "};",
                  "",
                  "let result = add(five, ten);",
                  "!-/*5;",
                  "5 < 10 > 5;",
                  "",
                  "if (5 < 10) {",
                  "   return true;",
                  "} else {",
                  "   return false;",
                  "}",
                  "",
                  "10 == 10;",
                  "10 != 9;",
                  "\"foobar\"",
                  "\"foo bar\"",
                  "[1, 2];",
                  "{\"foo\": \"bar\"}"
                ]
        expected =
            [ T.Token T.Let "let",
              T.Token T.Ident "five",
              T.Token T.Assign "=",
              T.Token T.Int "5",
              T.Token T.Semicolon ";",
              T.Token T.Let "let",
              T.Token T.Ident "ten",
              T.Token T.Assign "=",
              T.Token T.Int "10",
              T.Token T.Semicolon ";",
              T.Token T.Let "let",
              T.Token T.Ident "add",
              T.Token T.Assign "=",
              T.Token T.Function "fn",
              T.Token T.LParen "(",
              T.Token T.Ident "x",
              T.Token T.Comma ",",
              T.Token T.Ident "y",
              T.Token T.RParen ")",
              T.Token T.LBrace "{",
              T.Token T.Ident "x",
              T.Token T.Plus "+",
              T.Token T.Ident "y",
              T.Token T.Semicolon ";",
              T.Token T.RBrace "}",
              T.Token T.Semicolon ";",
              T.Token T.Let "let",
              T.Token T.Ident "result",
              T.Token T.Assign "=",
              T.Token T.Ident "add",
              T.Token T.LParen "(",
              T.Token T.Ident "five",
              T.Token T.Comma ",",
              T.Token T.Ident "ten",
              T.Token T.RParen ")",
              T.Token T.Semicolon ";",
              T.Token T.Bang "!",
              T.Token T.Minus "-",
              T.Token T.Slash "/",
              T.Token T.Asterisk "*",
              T.Token T.Int "5",
              T.Token T.Semicolon ";",
              T.Token T.Int "5",
              T.Token T.LessThan "<",
              T.Token T.Int "10",
              T.Token T.GreaterThan ">",
              T.Token T.Int "5",
              T.Token T.Semicolon ";",
              T.Token T.If "if",
              T.Token T.LParen "(",
              T.Token T.Int "5",
              T.Token T.LessThan "<",
              T.Token T.Int "10",
              T.Token T.RParen ")",
              T.Token T.LBrace "{",
              T.Token T.Return "return",
              T.Token T.True "true",
              T.Token T.Semicolon ";",
              T.Token T.RBrace "}",
              T.Token T.Else "else",
              T.Token T.LBrace "{",
              T.Token T.Return "return",
              T.Token T.False "false",
              T.Token T.Semicolon ";",
              T.Token T.RBrace "}",
              T.Token T.Int "10",
              T.Token T.Equal "==",
              T.Token T.Int "10",
              T.Token T.Semicolon ";",
              T.Token T.Int "10",
              T.Token T.NotEqual "!=",
              T.Token T.Int "9",
              T.Token T.Semicolon ";",
              T.Token T.String "foobar",
              T.Token T.String "foo bar",
              T.Token T.LBracket "[",
              T.Token T.Int "1",
              T.Token T.Comma ",",
              T.Token T.Int "2",
              T.Token T.RBracket "]",
              T.Token T.Semicolon ";",
              T.Token T.LBrace "{",
              T.Token T.String "foo",
              T.Token T.Colon ":",
              T.Token T.String "bar",
              T.Token T.RBrace "}",
              T.Token T.EOF ""
            ]
