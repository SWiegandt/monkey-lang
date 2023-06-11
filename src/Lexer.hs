{-# LANGUAGE TupleSections #-}

module Lexer (runLexer) where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadState (get, put), State, evalState, modify)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)
import qualified Tokens as T

skipWhitespace :: String -> String
skipWhitespace = dropWhile isSpace

nextToken :: State String T.Token
nextToken = do
    modify skipWhitespace
    s <- get
    case s of
        [] -> return $ T.Token T.EOF ""
        (c : s') -> do
            let peekT = (,tail s') <$> peekToken c s'
                charT = (,s') <$> charToken c
                stringT = first stringToken <$> identifier c s'
                numberT = numberToken c s'
                (token, rest) =
                    fromMaybe
                        (T.Token T.Illegal [c], s')
                        (peekT <|> charT <|> stringT <|> numberT)
            put rest
            return token

charType :: Char -> Maybe T.TokenType
charType ';' = Just T.Semicolon
charType '(' = Just T.LParen
charType ')' = Just T.RParen
charType ',' = Just T.Comma
charType '+' = Just T.Plus
charType '{' = Just T.LBrace
charType '}' = Just T.RBrace
charType '=' = Just T.Assign
charType '!' = Just T.Bang
charType '-' = Just T.Minus
charType '/' = Just T.Slash
charType '*' = Just T.Asterisk
charType '<' = Just T.LessThan
charType '>' = Just T.GreaterThan
charType _ = Nothing

charToken :: Char -> Maybe T.Token
charToken c = (`T.Token` [c]) <$> charType c

peekToken :: Char -> String -> Maybe T.Token
peekToken '=' ('=' : _) = Just $ T.Token T.Equal "=="
peekToken '!' ('=' : _) = Just $ T.Token T.NotEqual "!="
peekToken _ _ = Nothing

spanningMatcher :: (Char -> Bool) -> Char -> String -> Maybe (String, String)
spanningMatcher valid c s
    | valid c = let (matched, rest) = span valid s in Just (c : matched, rest)
    | otherwise = Nothing

identifier :: Char -> String -> Maybe (String, String)
identifier = spanningMatcher (\c -> isAlpha c || c == '_')

numberToken :: Char -> String -> Maybe (T.Token, String)
numberToken c s = first (T.Token T.Int) <$> spanningMatcher isDigit c s

keywordType :: String -> Maybe T.TokenType
keywordType "let" = Just T.Let
keywordType "fn" = Just T.Function
keywordType "true" = Just T.True
keywordType "false" = Just T.False
keywordType "if" = Just T.If
keywordType "else" = Just T.Else
keywordType "return" = Just T.Return
keywordType _ = Nothing

stringToken :: String -> T.Token
stringToken s = maybe (T.Token T.Ident s) (`T.Token` s) (keywordType s)

untilEOF :: State String [T.Token]
untilEOF = do
    token <- nextToken
    case T.ttype token of
        T.EOF -> return [token]
        _ -> (token :) <$> untilEOF

runLexer :: String -> [T.Token]
runLexer = evalState untilEOF
