module Evaluator where

import Control.Monad.State (MonadTrans (lift), StateT, gets, modify)
import qualified Data.Map as Map
import qualified Nodes as N
import qualified Object as O
import Text.Printf (printf)

type ProgramOutput = Either String O.Object

type ProgramState = StateT (Map.Map String O.Object) (Either String)

eval :: N.Program -> ProgramState O.Object
eval (N.Program p) = evalProgram p

evalStatements :: (O.Object -> O.Object) -> [N.Statement] -> ProgramState O.Object
evalStatements unwrap [] = lift $ Right O.ONull
evalStatements unwrap (s : ss) = do
    evaluation <- evalStatement s
    case evaluation of
        o@(O.OReturn _) -> return $ unwrap o
        o -> if null ss then return (unwrap o) else evalStatements unwrap ss

evalProgram :: [N.Statement] -> ProgramState O.Object
evalProgram = evalStatements go
    where
        go (O.OReturn o) = o
        go obj = obj

evalBlock :: N.Block -> ProgramState O.Object
evalBlock (N.Block stmts) = evalStatements id stmts

evalStatement :: N.Statement -> ProgramState O.Object
evalStatement (N.LetStmt _ (N.Identifier _ name) value) = do
    evaluation <- evalExpression value
    modify (Map.insert name evaluation)
    return evaluation
evalStatement (N.ReturnStmt _ expr) = O.OReturn <$> evalExpression expr
evalStatement (N.ExpressionStmt _ expr) = evalExpression expr

evalExpression :: N.Expression -> ProgramState O.Object
evalExpression (N.IntegerExpression _ v) = lift . Right $ O.OInt v
evalExpression (N.BooleanExpression _ v) = lift . Right $ O.OBool v
evalExpression (N.PrefixExpression _ op expr) = evalExpression expr >>= evalPrefixExpression op
evalExpression (N.InfixExpression _ lhs op rhs) = do
    lhsEval <- evalExpression lhs
    rhsEval <- evalExpression rhs
    evalInfixExpression lhsEval op rhsEval
evalExpression (N.IfExpression _ cond cons alt) = evalIfExpression cond cons alt
evalExpression (N.IdentifierExpression (N.Identifier _ name)) = do
    value <- gets (Map.!? name)
    case value of
        Just v -> lift $ Right v
        Nothing -> lift . Left $ printf "identifier not found: %s" name
evalExpression _ = lift $ Right O.ONull

evalPrefixExpression :: String -> O.Object -> ProgramState O.Object
evalPrefixExpression "!" obj = evalBangOperatorExpression obj
evalPrefixExpression "-" obj = evalMinusPrefixOperatorExpression obj
evalPrefixExpression s obj = lift . Left $ printf "unknown operator: %s%s" s (show $ O.otype obj)

evalBangOperatorExpression :: O.Object -> ProgramState O.Object
evalBangOperatorExpression (O.OBool b) = lift . Right $ O.OBool (not b)
evalBangOperatorExpression O.ONull = lift . Right $ O.OBool True
evalBangOperatorExpression _ = lift . Right $ O.OBool False

evalMinusPrefixOperatorExpression :: O.Object -> ProgramState O.Object
evalMinusPrefixOperatorExpression (O.OInt v) = lift . Right $ O.OInt (-v)
evalMinusPrefixOperatorExpression obj = lift . Left $ printf "unknown operator: -%s" (show $ O.otype obj)

evalInfixExpression :: O.Object -> String -> O.Object -> ProgramState O.Object
evalInfixExpression l@(O.OInt _) op r@(O.OInt _) = evalIntegerInfixExpression l op r
evalInfixExpression l "==" r = lift . Right $ O.OBool (l == r)
evalInfixExpression l "!=" r = lift . Right $ O.OBool (l /= r)
evalInfixExpression l op r =
    if O.otype l /= O.otype r
        then lift . Left $ printf "type mismatch: %s %s %s" (show $ O.otype l) op (show $ O.otype r)
        else lift . Left $ printf "unknown operator: %s %s %s" (show $ O.otype l) op (show $ O.otype r)

evalIntegerInfixExpression :: O.Object -> String -> O.Object -> ProgramState O.Object
evalIntegerInfixExpression (O.OInt n) "+" (O.OInt m) = lift . Right $ O.OInt (n + m)
evalIntegerInfixExpression (O.OInt n) "-" (O.OInt m) = lift . Right $ O.OInt (n - m)
evalIntegerInfixExpression (O.OInt n) "*" (O.OInt m) = lift . Right $ O.OInt (n * m)
evalIntegerInfixExpression (O.OInt n) "/" (O.OInt m) = lift . Right $ O.OInt (n `div` m)
evalIntegerInfixExpression (O.OInt n) "<" (O.OInt m) = lift . Right $ O.OBool (n < m)
evalIntegerInfixExpression (O.OInt n) ">" (O.OInt m) = lift . Right $ O.OBool (n > m)
evalIntegerInfixExpression (O.OInt n) "==" (O.OInt m) = lift . Right $ O.OBool (n == m)
evalIntegerInfixExpression (O.OInt n) "!=" (O.OInt m) = lift . Right $ O.OBool (n /= m)
evalIntegerInfixExpression l op r = lift . Left $ printf "unknown operator: %s %s %s" (show $ O.otype l) op (show $ O.otype r)

evalIfExpression :: N.Expression -> N.Block -> Maybe N.Block -> ProgramState O.Object
evalIfExpression cond cons alt = do
    evaluation <- evalExpression cond
    if isTruthy evaluation
        then evalBlock cons
        else maybe (return O.ONull) evalBlock alt

isTruthy :: O.Object -> Bool
isTruthy O.ONull = False
isTruthy (O.OBool b) = b
isTruthy _ = True
