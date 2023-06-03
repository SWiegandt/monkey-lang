{-# LANGUAGE TupleSections #-}

module Lexer (Token (..), TokenType (..), runLexer) where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadState (get, put, state), State, evalState, modify)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)

data TokenType
    = Illegal
    | EOF
    | Ident
    | Int
    | Assign
    | Plus
    | Minus
    | Bang
    | Slash
    | Asterisk
    | LessThan
    | GreaterThan
    | Equal
    | NotEqual
    | Comma
    | Semicolon
    | LParen
    | RParen
    | LBrace
    | RBrace
    | Function
    | Let
    | TrueT
    | FalseT
    | If
    | Else
    | Return
    deriving (Show, Eq)

data Token = Token {ttype :: TokenType, literal :: String} deriving (Show, Eq)

skipWhitespace :: String -> String
skipWhitespace = dropWhile isSpace

nextToken :: State String Token
nextToken = do
    modify skipWhitespace
    s <- get
    case s of
        [] -> return $ Token EOF ""
        (c : s') -> do
            let peekT = (,tail s') <$> peekToken c s'
                charT = (,s') <$> charToken c
                stringT = first stringToken <$> identifier c s'
                numberT = numberToken c s'
                (token, rest) =
                    fromMaybe
                        (Token Illegal [c], s')
                        (peekT <|> charT <|> stringT <|> numberT)
            put rest
            return token

charType :: Char -> Maybe TokenType
charType ';' = Just Semicolon
charType '(' = Just LParen
charType ')' = Just RParen
charType ',' = Just Comma
charType '+' = Just Plus
charType '{' = Just LBrace
charType '}' = Just RBrace
charType '=' = Just Assign
charType '!' = Just Bang
charType '-' = Just Minus
charType '/' = Just Slash
charType '*' = Just Asterisk
charType '<' = Just LessThan
charType '>' = Just GreaterThan
charType _ = Nothing

charToken :: Char -> Maybe Token
charToken c = (`Token` [c]) <$> charType c

peekToken :: Char -> String -> Maybe Token
peekToken '=' ('=' : _) = Just $ Token Equal "=="
peekToken '!' ('=' : _) = Just $ Token NotEqual "!="
peekToken _ _ = Nothing

spanningMatcher :: (Char -> Bool) -> Char -> String -> Maybe (String, String)
spanningMatcher valid c s
    | valid c = let (matched, rest) = span valid s in Just (c : matched, rest)
    | otherwise = Nothing

identifier :: Char -> String -> Maybe (String, String)
identifier = spanningMatcher (\c -> isAlpha c || c == '_')

numberToken :: Char -> String -> Maybe (Token, String)
numberToken c s = first (Token Int) <$> spanningMatcher isDigit c s

keywordType :: String -> Maybe TokenType
keywordType "let" = Just Let
keywordType "fn" = Just Function
keywordType "true" = Just TrueT
keywordType "false" = Just FalseT
keywordType "if" = Just If
keywordType "else" = Just Else
keywordType "return" = Just Return
keywordType _ = Nothing

stringToken :: String -> Token
stringToken s = maybe (Token Ident s) (`Token` s) (keywordType s)

untilEOF :: State String [Token]
untilEOF = do
    token <- nextToken
    case ttype token of
        EOF -> return [token]
        _ -> (token :) <$> untilEOF

runLexer :: String -> [Token]
runLexer = evalState untilEOF
