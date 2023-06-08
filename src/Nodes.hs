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
    | MissingExpr
    deriving (Eq)

instance Node Expression where
    tokenLiteral (IdentifierExpr i) = tokenLiteral i
    tokenLiteral (IntegerLiteralExpr t _) = T.literal t
    tokenLiteral MissingExpr = ""

instance Show Expression where
    show (IdentifierExpr i) = show i
    show e@(IntegerLiteralExpr {}) = tokenLiteral e
    show MissingExpr = ""

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
