{-# LANGUAGE TupleSections #-}

module Lexer where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadState (get, put, state), State, evalState)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Prelude hiding (EQ, False, GT, LT, True)

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
  | LT
  | GT
  | EQ
  | NEQ
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  deriving (Show, Eq)

data Token = Token {ttype :: TokenType, literal :: String} deriving (Show, Eq)

char :: State String Char
char = state $ \s -> let (c : s') = s in (c, s')

skipWhitespace :: State String ()
skipWhitespace = state $ \s -> ((), dropWhile isSpace s)

nextToken :: State String Token
nextToken = do
  skipWhitespace
  s <- get
  case s of
    [] -> return $ Token EOF ""
    (c : s') -> do
      let peekToken = (,tail s') <$> peek c s'
      let charToken = (,s') <$> tokenFromChar c
      let identToken = first identifierToken <$> identifier c s'
      let numToken = number c s'

      let (token, rest) =
            fromMaybe
              (Token Illegal [c], s')
              ( peekToken
                  <|> charToken
                  <|> identToken
                  <|> numToken
              )
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
tokenFromChar '<' = Just $ Token LT "<"
tokenFromChar '>' = Just $ Token GT ">"
tokenFromChar _ = Nothing

peek :: Char -> String -> Maybe Token
peek '=' ('=' : _) = Just (Token EQ "==")
peek '!' ('=' : _) = Just (Token NEQ "!=")
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
identifierToken "true" = Token True "true"
identifierToken "false" = Token False "false"
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
