module Evaluator where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT)
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Nodes as N
import qualified Object as O
import Text.Printf (printf)

type ProgramOutput = Either String O.Object

type ProgramState = StateT (IORef O.Environment) (ExceptT String IO)

eval :: N.Program -> ProgramState O.Object
eval (N.Program p) = evalProgram p

evalStatements :: (O.Object -> O.Object) -> [N.Statement] -> ProgramState O.Object
evalStatements unwrap [] = return O.ONull
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
    env <- get
    liftIO $ modifyIORef' env (O.insert name evaluation)
    return evaluation
evalStatement (N.ReturnStmt _ expr) = O.OReturn <$> evalExpression expr
evalStatement (N.ExpressionStmt _ expr) = evalExpression expr

evalExpression :: N.Expression -> ProgramState O.Object
evalExpression (N.IntegerExpression _ v) = return $ O.OInt v
evalExpression (N.BooleanExpression _ v) = return $ O.OBool v
evalExpression (N.PrefixExpression _ op expr) = evalExpression expr >>= evalPrefixExpression op
evalExpression (N.InfixExpression _ lhs op rhs) = do
    lhsEval <- evalExpression lhs
    rhsEval <- evalExpression rhs
    evalInfixExpression lhsEval op rhsEval
evalExpression (N.IfExpression _ cond cons alt) = evalIfExpression cond cons alt
evalExpression (N.IdentifierExpression (N.Identifier _ name)) = do
    env <- get >>= liftIO . readIORef
    value <- liftIO $ env O.!? name
    case value of
        Just v -> return v
        Nothing -> throwError $ printf "identifier not found: %s" name
evalExpression _ = return O.ONull

evalPrefixExpression :: String -> O.Object -> ProgramState O.Object
evalPrefixExpression "!" obj = evalBangOperatorExpression obj
evalPrefixExpression "-" obj = evalMinusPrefixOperatorExpression obj
evalPrefixExpression s obj = throwError $ printf "unknown operator: %s%s" s (show $ O.otype obj)

evalBangOperatorExpression :: O.Object -> ProgramState O.Object
evalBangOperatorExpression (O.OBool b) = return $ O.OBool (not b)
evalBangOperatorExpression O.ONull = return $ O.OBool True
evalBangOperatorExpression _ = return $ O.OBool False

evalMinusPrefixOperatorExpression :: O.Object -> ProgramState O.Object
evalMinusPrefixOperatorExpression (O.OInt v) = return $ O.OInt (-v)
evalMinusPrefixOperatorExpression obj = throwError $ printf "unknown operator: -%s" (show $ O.otype obj)

evalInfixExpression :: O.Object -> String -> O.Object -> ProgramState O.Object
evalInfixExpression l@(O.OInt _) op r@(O.OInt _) = evalIntegerInfixExpression l op r
evalInfixExpression l "==" r = return $ O.OBool (l == r)
evalInfixExpression l "!=" r = return $ O.OBool (l /= r)
evalInfixExpression l op r =
    if O.otype l /= O.otype r
        then throwError $ printf "type mismatch: %s %s %s" (show $ O.otype l) op (show $ O.otype r)
        else throwError $ printf "unknown operator: %s %s %s" (show $ O.otype l) op (show $ O.otype r)

evalIntegerInfixExpression :: O.Object -> String -> O.Object -> ProgramState O.Object
evalIntegerInfixExpression (O.OInt n) "+" (O.OInt m) = return $ O.OInt (n + m)
evalIntegerInfixExpression (O.OInt n) "-" (O.OInt m) = return $ O.OInt (n - m)
evalIntegerInfixExpression (O.OInt n) "*" (O.OInt m) = return $ O.OInt (n * m)
evalIntegerInfixExpression (O.OInt n) "/" (O.OInt m) = return $ O.OInt (n `div` m)
evalIntegerInfixExpression (O.OInt n) "<" (O.OInt m) = return $ O.OBool (n < m)
evalIntegerInfixExpression (O.OInt n) ">" (O.OInt m) = return $ O.OBool (n > m)
evalIntegerInfixExpression (O.OInt n) "==" (O.OInt m) = return $ O.OBool (n == m)
evalIntegerInfixExpression (O.OInt n) "!=" (O.OInt m) = return $ O.OBool (n /= m)
evalIntegerInfixExpression l op r = throwError $ printf "unknown operator: %s %s %s" (show $ O.otype l) op (show $ O.otype r)

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
