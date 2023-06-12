module Nodes where

import Data.List (intercalate)
import Text.Printf (printf)
import qualified Tokens as T

class Node a where
    tokenLiteral :: a -> String

data Identifier = Identifier
    { identToken :: T.Token,
      identValue :: String
    }
    deriving (Eq)

instance Node Identifier where
    tokenLiteral = T.literal . identToken

instance Show Identifier where
    show = identValue

data Expression
    = IdentifierExpression Identifier
    | IntegerExpression T.Token Integer
    | BooleanExpression T.Token Bool
    | PrefixExpression T.Token String Expression
    | InfixExpression T.Token Expression String Expression
    | IfExpression T.Token Expression Block (Maybe Block)
    | FunctionExpression T.Token [Identifier] Block
    | CallExpression T.Token Expression [Expression]
    deriving (Eq)

instance Node Expression where
    tokenLiteral (IdentifierExpression i) = tokenLiteral i
    tokenLiteral (IntegerExpression t _) = T.literal t
    tokenLiteral (BooleanExpression t _) = T.literal t
    tokenLiteral (PrefixExpression t _ _) = T.literal t
    tokenLiteral (InfixExpression t _ _ _) = T.literal t
    tokenLiteral (IfExpression t _ _ _) = T.literal t
    tokenLiteral (FunctionExpression t _ _) = T.literal t
    tokenLiteral (CallExpression t _ _) = T.literal t

instance Show Expression where
    show (IdentifierExpression i) = show i
    show e@(IntegerExpression {}) = tokenLiteral e
    show e@(BooleanExpression {}) = tokenLiteral e
    show (PrefixExpression _ op rhs) = printf "(%s%s)" op (show rhs)
    show (InfixExpression _ lhs op rhs) = printf "(%s %s %s)" (show lhs) op (show rhs)
    show (IfExpression _ cond cons alt) =
        let altString = maybe "" (printf "else %s" . show) alt
         in printf "if%s %s%s" (show cond) (show cons) altString
    show f@(FunctionExpression _ params body) =
        printf
            "%s(%s) %s"
            (tokenLiteral f)
            (intercalate ", " $ map show params)
            (show body)
    show c@(CallExpression _ fn args) = printf "%s(%s)" (show fn) (intercalate ", " $ map show args)

newtype Block = Block [Statement] deriving (Eq)

instance Show Block where
    show (Block stmts) = concatMap show stmts

data Statement
    = LetStmt
        { letToken :: T.Token,
          letName :: Identifier,
          letValue :: Expression
        }
    | ReturnStmt
        { returnToken :: T.Token,
          returnValue :: Expression
        }
    | ExpressionStmt
        { expressionToken :: T.Token,
          expression :: Expression
        }
    deriving (Eq)

instance Node Statement where
    tokenLiteral (LetStmt t _ _) = T.literal t
    tokenLiteral (ReturnStmt t _) = T.literal t
    tokenLiteral (ExpressionStmt t _) = T.literal t

instance Show Statement where
    show stmt@(LetStmt _ n v) = printf "%s %s = %s;" (tokenLiteral stmt) (show n) (show v)
    show stmt@(ReturnStmt _ v) = printf "%s %s;" (tokenLiteral stmt) (show v)
    show (ExpressionStmt _ e) = show e

newtype Program = Program [Statement]

instance Node Program where
    tokenLiteral (Program []) = ""
    tokenLiteral (Program (s : _)) = tokenLiteral s

instance Show Program where
    show (Program stmts) = concatMap show stmts
