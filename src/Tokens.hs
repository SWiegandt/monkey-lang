module Tokens (TokenType (..), Token (..)) where

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
    | Colon
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    | Function
    | Let
    | True
    | False
    | If
    | Else
    | Return
    | String
    deriving (Show, Eq)

data Token = Token {ttype :: TokenType, literal :: String} deriving (Show, Eq)
