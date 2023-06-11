module Nodes where

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
    = IdentifierExpr Identifier
    | IntegerLiteralExpr T.Token Integer
    | BooleanExpr T.Token Bool
    | PrefixExpression T.Token String Expression
    | InfixExpression T.Token Expression String Expression
    | IfExpression T.Token Expression Block (Maybe Block)
    | MissingExpr
    deriving (Eq)

instance Node Expression where
    tokenLiteral (IdentifierExpr i) = tokenLiteral i
    tokenLiteral (IntegerLiteralExpr t _) = T.literal t
    tokenLiteral (BooleanExpr t _) = T.literal t
    tokenLiteral (PrefixExpression t _ _) = T.literal t
    tokenLiteral (InfixExpression t _ _ _) = T.literal t
    tokenLiteral (IfExpression t _ _ _) = T.literal t
    tokenLiteral MissingExpr = ""

instance Show Expression where
    show (IdentifierExpr i) = show i
    show e@(IntegerLiteralExpr {}) = tokenLiteral e
    show e@(BooleanExpr {}) = tokenLiteral e
    show (PrefixExpression _ op rhs) = printf "(%s%s)" op (show rhs)
    show (InfixExpression _ lhs op rhs) = printf "(%s %s %s)" (show lhs) op (show rhs)
    show (IfExpression _ cond cons alt) =
        let altString = maybe "" (printf "else %s" . show) alt
         in printf "if%s %s%s" (show cond) (show cons) altString
    show MissingExpr = ""

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
    | BlockStatement
        { blockToken :: T.Token,
          block :: Block
        }
    | InvalidStatement
    deriving (Eq)

instance Node Statement where
    tokenLiteral (LetStmt t _ _) = T.literal t
    tokenLiteral (ReturnStmt t _) = T.literal t
    tokenLiteral (ExpressionStmt t _) = T.literal t
    tokenLiteral (BlockStatement t _) = T.literal t

instance Show Statement where
    show stmt@(LetStmt _ n v) = printf "%s %s = %s;" (tokenLiteral stmt) (show n) (show v)
    show stmt@(ReturnStmt _ v) = printf "%s %s;" (tokenLiteral stmt) (show v)
    show (ExpressionStmt _ e) = show e
    show (BlockStatement _ b) = show b
    show InvalidStatement = "INVALID"
