{-# LANGUAGE TupleSections #-}

module Lexer where

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
            let peekToken = (,tail s') <$> peek c s'
                charToken = (,s') <$> tokenFromChar c
                identToken = first identifierToken <$> identifier c s'
                numToken = number c s'

                (token, rest) =
                    fromMaybe
                        (Token Illegal [c], s')
                        (peekToken <|> charToken <|> identToken <|> numToken)
            put rest
            return token

tokenFromChar :: Char -> Maybe Token
tokenFromChar ';' = Just $ Token Semicolon ";"
tokenFromChar '(' = Just $ Token LParen "("
tokenFromChar ')' = Just $ Token RParen ")"
tokenFromChar ',' = Just $ Token Comma ","
tokenFromChar '+' = Just $ Token Plus "+"
tokenFromChar '{' = Just $ Token LBrace "{"
tokenFromChar '}' = Just $ Token RBrace "}"
tokenFromChar '=' = Just $ Token Assign "="
tokenFromChar '!' = Just $ Token Bang "!"
tokenFromChar '-' = Just $ Token Minus "-"
tokenFromChar '/' = Just $ Token Slash "/"
tokenFromChar '*' = Just $ Token Asterisk "*"
tokenFromChar '<' = Just $ Token LessThan "<"
tokenFromChar '>' = Just $ Token GreaterThan ">"
tokenFromChar _ = Nothing

peek :: Char -> String -> Maybe Token
peek '=' ('=' : _) = Just $ Token Equal "=="
peek '!' ('=' : _) = Just $ Token NotEqual "!="
peek _ _ = Nothing

spanningToken :: (Char -> Bool) -> Char -> String -> Maybe (String, String)
spanningToken valid c s
    | valid c = let (matched, rest) = span valid s in Just (c : matched, rest)
    | otherwise = Nothing

identifier :: Char -> String -> Maybe (String, String)
identifier = spanningToken (\c -> isAlpha c || c == '_')

number :: Char -> String -> Maybe (Token, String)
number c s = first (Token Int) <$> spanningToken isDigit c s

identifierToken :: String -> Token
identifierToken "let" = Token Let "let"
identifierToken "fn" = Token Function "fn"
identifierToken "true" = Token TrueT "true"
identifierToken "false" = Token FalseT "false"
identifierToken "if" = Token If "if"
identifierToken "else" = Token Else "else"
identifierToken "return" = Token Return "return"
identifierToken s = Token Ident s

untilEOF :: State String [Token]
untilEOF = do
    token <- nextToken
    case ttype token of
        EOF -> return [token]
        _ -> (token :) <$> untilEOF

runLexer :: String -> [Token]
runLexer = evalState untilEOF
