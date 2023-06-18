module Evaluator where

import qualified Builtins as B
import Control.Applicative ((<|>))
import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadIO (liftIO), MonadState (get, put), gets)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (genericLength)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Nodes as N
import qualified Object as O
import Text.Printf (printf)

type ProgramOutput = Either String O.Object

eval :: N.Program -> O.ProgramState O.Object
eval (N.Program p) = evalProgram p

evalProgram :: [N.Statement] -> O.ProgramState O.Object
evalProgram = evalStatements unwrap
    where
        unwrap (O.Return o) = o
        unwrap obj = obj

evalBlock :: N.Block -> O.ProgramState O.Object
evalBlock (N.Block stmts) = evalStatements id stmts

evalStatements :: (O.Object -> O.Object) -> [N.Statement] -> O.ProgramState O.Object
evalStatements unwrap [] = return O.Null
evalStatements unwrap (s : ss) = do
    evaluation <- evalStatement s
    case evaluation of
        o@(O.Return _) -> return $ unwrap o
        o -> if null ss then return (unwrap o) else evalStatements unwrap ss

evalStatement :: N.Statement -> O.ProgramState O.Object
evalStatement (N.LetStmt _ (N.Identifier _ name) value) = do
    evaluation <- evalExpression value
    O.EnvRef env <- get
    liftIO $ modifyIORef' env (O.insert name evaluation)
    return O.Null
evalStatement (N.ReturnStmt _ expr) = O.Return <$> evalExpression expr
evalStatement (N.ExpressionStmt _ expr) = evalExpression expr

readEnv :: O.ProgramState O.Environment
readEnv = get >>= liftIO . readIORef . O.unRef

evalExpression :: N.Expression -> O.ProgramState O.Object
evalExpression (N.IntegerExpression _ v) = return $ O.Int v
evalExpression (N.BooleanExpression _ v) = return $ O.Bool v
evalExpression (N.StringExpression _ v) = return $ O.String v
evalExpression (N.PrefixExpression _ op expr) = evalExpression expr >>= evalPrefixExpression op
evalExpression (N.InfixExpression _ lhs op rhs) = do
    lhsEval <- evalExpression lhs
    rhsEval <- evalExpression rhs
    evalInfixExpression lhsEval op rhsEval
evalExpression (N.IfExpression _ cond cons alt) = evalIfExpression cond cons alt
evalExpression (N.IdentifierExpression (N.Identifier _ name)) = do
    env <- readEnv
    value <- liftIO $ (<|> B.builtins Map.!? name) <$> env O.!? name
    case value of
        Just v -> return v
        Nothing -> throwError (printf "identifier not found: %s" name)
evalExpression (N.FunctionExpression _ params body) = gets (O.Function params body)
evalExpression (N.CallExpression _ funcExpr argExprs) = do
    func <- evalExpression funcExpr
    args <- mapM evalExpression argExprs
    applyFunction func args
evalExpression (N.ArrayExpression _ elements) = O.Array <$> mapM evalExpression elements
evalExpression (N.IndexExpression _ lhs rhs) = do
    collection <- evalExpression lhs
    index <- evalExpression rhs
    evalIndexExpression collection index
evalExpression (N.HashExpression _ map) = O.Hash . Map.fromList <$> forM (Map.toList map) (uncurry evalHashKeyValue)

locally :: (MonadState s m) => s -> m a -> m a
locally s m = do
    st <- get
    put s
    result <- m
    put st
    return result

applyFunction :: O.Object -> [O.Object] -> O.ProgramState O.Object
applyFunction func@(O.Function params body env) args = do
    extended <- liftIO $ extendEnvironment env params args
    let unwrap (O.Return o) = o
        unwrap o = o
    locally extended $ unwrap <$> evalBlock body
applyFunction builtin@(O.Builtin {}) args = B.runBuiltin builtin args
applyFunction o _ = throwError $ printf "not a function: %s" (O.inspect o)

extendEnvironment :: O.EnvironmentRef -> [N.Identifier] -> [O.Object] -> IO O.EnvironmentRef
extendEnvironment env params args = do
    let funcEnv = Map.fromList (zip (map show params) args)
    O.EnvRef <$> newIORef (O.Env funcEnv (Just env))

evalPrefixExpression :: String -> O.Object -> O.ProgramState O.Object
evalPrefixExpression "!" obj = evalBangOperatorExpression obj
evalPrefixExpression "-" obj = evalMinusPrefixOperatorExpression obj
evalPrefixExpression s obj = throwError $ printf "unknown operator: %s%s" s (show $ O.otype obj)

evalBangOperatorExpression :: O.Object -> O.ProgramState O.Object
evalBangOperatorExpression (O.Bool b) = return $ O.Bool (not b)
evalBangOperatorExpression O.Null = return $ O.Bool True
evalBangOperatorExpression _ = return $ O.Bool False

evalMinusPrefixOperatorExpression :: O.Object -> O.ProgramState O.Object
evalMinusPrefixOperatorExpression (O.Int v) = return $ O.Int (-v)
evalMinusPrefixOperatorExpression obj = throwError $ printf "unknown operator: -%s" (show $ O.otype obj)

evalInfixExpression :: O.Object -> String -> O.Object -> O.ProgramState O.Object
evalInfixExpression l@(O.Int _) op r@(O.Int _) = evalIntegerInfixExpression l op r
evalInfixExpression l@(O.String _) op r@(O.String _) = evalStringInfixExpression l op r
evalInfixExpression l op r = evalGeneralInfixExpression l op r

evalGeneralInfixExpression :: O.Object -> String -> O.Object -> O.ProgramState O.Object
evalGeneralInfixExpression l "==" r = return $ O.Bool (l == r)
evalGeneralInfixExpression l "!=" r = return $ O.Bool (l /= r)
evalGeneralInfixExpression l op r =
    if O.otype l /= O.otype r
        then throwError $ printf "type mismatch: %s %s %s" (show $ O.otype l) op (show $ O.otype r)
        else throwError $ printf "unknown operator: %s %s %s" (show $ O.otype l) op (show $ O.otype r)

evalIntegerInfixExpression :: O.Object -> String -> O.Object -> O.ProgramState O.Object
evalIntegerInfixExpression (O.Int n) "+" (O.Int m) = return $ O.Int (n + m)
evalIntegerInfixExpression (O.Int n) "-" (O.Int m) = return $ O.Int (n - m)
evalIntegerInfixExpression (O.Int n) "*" (O.Int m) = return $ O.Int (n * m)
evalIntegerInfixExpression (O.Int n) "/" (O.Int m) = return $ O.Int (n `div` m)
evalIntegerInfixExpression (O.Int n) "<" (O.Int m) = return $ O.Bool (n < m)
evalIntegerInfixExpression (O.Int n) ">" (O.Int m) = return $ O.Bool (n > m)
evalIntegerInfixExpression (O.Int n) "==" (O.Int m) = return $ O.Bool (n == m)
evalIntegerInfixExpression (O.Int n) "!=" (O.Int m) = return $ O.Bool (n /= m)
evalIntegerInfixExpression l op r = evalGeneralInfixExpression l op r

evalStringInfixExpression :: O.Object -> String -> O.Object -> O.ProgramState O.Object
evalStringInfixExpression (O.String l) "+" (O.String r) = return $ O.String (l ++ r)
evalStringInfixExpression l op r = evalGeneralInfixExpression l op r

evalIfExpression :: N.Expression -> N.Block -> Maybe N.Block -> O.ProgramState O.Object
evalIfExpression cond cons alt = do
    evaluation <- evalExpression cond
    if isTruthy evaluation
        then evalBlock cons
        else maybe (return O.Null) evalBlock alt

evalIndexExpression :: O.Object -> O.Object -> O.ProgramState O.Object
evalIndexExpression (O.Array elements) (O.Int n)
    | n < 0 || n >= genericLength elements = return O.Null
    | otherwise = return $ elements !! fromInteger n
evalIndexExpression (O.Hash map) o
    | O.keyable o = return . fromMaybe O.Null $ map Map.!? o
    | otherwise = throwError $ printf "unusable as hash key: %s" (show $ O.otype o)
evalIndexExpression o _ = throwError $ printf "index operator not supported: %s" (show $ O.otype o)

evalHashKeyValue :: N.Expression -> N.Expression -> O.ProgramState (O.Object, O.Object)
evalHashKeyValue k v = do
    key <- evalExpression k
    unless (O.keyable key) $ throwError $ printf "unusable as hash key: %s" (show $ O.otype key)
    value <- evalExpression v
    return (key, value)

isTruthy :: O.Object -> Bool
isTruthy O.Null = False
isTruthy (O.Bool b) = b
isTruthy _ = True
