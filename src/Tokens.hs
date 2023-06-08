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
