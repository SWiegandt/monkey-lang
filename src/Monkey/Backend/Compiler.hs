module Monkey.Backend.Compiler where

import Control.Monad (foldM)
import Monkey.AST.Nodes (
    Expression (InfixExpression, IntegerExpression),
    Node,
    Program (Program),
    Statement (ExpressionStmt),
 )
import Monkey.Backend.Code (Instructions (Instructions), make, opConstant)
import Monkey.Eval.Object (Object (Int))

data Bytecode = Bytecode Instructions [Object]

instance Semigroup Bytecode where
    Bytecode i1 c1 <> Bytecode i2 c2 = Bytecode (i1 <> i2) (c1 <> c2)

instance Monoid Bytecode where
    mempty = Bytecode (Instructions []) []

class (Node a) => Compiler a where
    compile :: Int -> a -> Either String Bytecode

instance Compiler Program where
    compile pos (Program p) = snd <$> foldM go (0, mempty) p
        where
            go (pos, bytecode) stmt = do
                compiled@(Bytecode _ c) <- compile pos stmt
                return (pos + length c, bytecode <> compiled)

instance Compiler Statement where
    compile pos (ExpressionStmt _ expr) = compile pos expr
    compile _ _ = error "can't compile statement"

instance Compiler Expression where
    compile pos (InfixExpression _ lhs _ rhs) = mappend <$> compile pos lhs <*> compile (pos + 1) rhs
    compile pos (IntegerExpression _ i) = Right $ Bytecode (make opConstant [pos]) [Int i]
    compile _ _ = error "can't compile expression"
