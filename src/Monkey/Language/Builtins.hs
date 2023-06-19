{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Monkey.Language.Builtins where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Data.List (genericLength)
import qualified Data.Map.Strict as Map
import qualified Monkey.Eval.Object as O
import Text.Printf (printf)

builtins :: Map.Map String O.Object
builtins =
    Map.fromList
        [ ("len", len),
          ("first", first),
          ("last", last'),
          ("rest", rest),
          ("push", push),
          ("add", add),
          ("puts", puts)
        ]

checkArgumentLength :: O.Object -> [O.Object] -> O.ProgramState O.Object
checkArgumentLength f@(O.Builtin _ n _ (O.BuiltinFunc _)) args
    | n == -1 || n == length args = liftEither $ Right f
    | otherwise = throwError $ printf "wrong number of arguments. got=%s, want=%s" (show $ length args) (show n)

checkArgumentTypes :: O.Object -> [O.Object] -> O.ProgramState O.Object
checkArgumentTypes f@(O.Builtin _ _ types (O.BuiltinFunc _)) args
    | and $ zipWith (\tpe arg -> null tpe || O.otype arg `elem` tpe) types args = liftEither $ Right f
    | otherwise = throwError $ printf "argument to `len` not supported, got %s" (show . O.otype $ head args)

runBuiltin :: O.Object -> [O.Object] -> O.ProgramState O.Object
runBuiltin f@(O.Builtin _ _ _ (O.BuiltinFunc impl)) args = do
    checkArgumentLength f args
    checkArgumentTypes f args
    impl args

mkBuiltin :: String -> Int -> [[O.ObjectType]] -> ([O.Object] -> O.ProgramState O.Object) -> O.Object
mkBuiltin name argc argTypes impl = O.Builtin name argc argTypes $ O.BuiltinFunc impl

len = mkBuiltin "len" 1 [[O.StringType, O.ArrayType]] (return . impl)
    where
        impl [O.String str] = O.Int $ genericLength str
        impl [O.Array arr] = O.Int $ genericLength arr

first = mkBuiltin "first" 1 [[O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array (e : _)] = e

last' = mkBuiltin "last" 1 [[O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = last elements

rest = mkBuiltin "rest" 1 [[O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = O.Array $ tail elements

push = mkBuiltin "push" 2 [[O.ArrayType], []] (return . impl)
    where
        impl [O.Array elements, o] = O.Array $ elements ++ [o]

add = mkBuiltin "add" 3 [[O.HashType], [O.IntegerType, O.BooleanType, O.StringType], []] (return . impl)
    where
        impl [O.Hash map, key, value] = O.Hash $ Map.insert key value map

puts = mkBuiltin "puts" (-1) [[]] impl
    where
        impl os = liftIO (mapM_ (putStrLn . O.inspect) os) >> return O.Null
