{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Builtins where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Data.List (genericLength)
import qualified Data.Map as Map
import qualified Object as O
import Text.Printf (printf)

builtins :: Map.Map String O.Object
builtins =
    Map.fromList
        [ ("len", len),
          ("first", first),
          ("last", last'),
          ("rest", rest),
          ("push", push),
          ("puts", puts)
        ]

checkArgumentLength :: O.Object -> [O.Object] -> O.ProgramState O.Object
checkArgumentLength f@(O.Builtin (O.BuiltinFunc _ n _ _)) args
    | n == -1 || n == length args = liftEither $ Right f
    | otherwise = throwError $ printf "wrong number of arguments. got=%s, want=%s" (show $ length args) (show n)

checkArgumentTypes :: O.Object -> [O.Object] -> O.ProgramState O.Object
checkArgumentTypes f@(O.Builtin (O.BuiltinFunc _ _ types _)) args
    | and $ zipWith (\tpe arg -> Nothing `elem` tpe || Just (O.otype arg) `elem` tpe) types args = liftEither $ Right f
    | otherwise = throwError $ printf "argument to `len` not supported, got %s" (show . O.otype $ head args)

runBuiltin :: O.Object -> [O.Object] -> O.ProgramState O.Object
runBuiltin f@(O.Builtin (O.BuiltinFunc _ _ _ impl)) args = do
    checkArgumentLength f args
    checkArgumentTypes f args
    liftIO $ impl args

mkBuiltin :: String -> Int -> [[Maybe O.ObjectType]] -> ([O.Object] -> IO O.Object) -> O.Object
mkBuiltin name argc argTypes impl = O.Builtin $ O.BuiltinFunc name argc argTypes impl

len = mkBuiltin "len" 1 [[Just O.StringType, Just O.ArrayType]] (return . impl)
    where
        impl [O.String str] = O.Int $ genericLength str
        impl [O.Array arr] = O.Int $ genericLength arr

first = mkBuiltin "first" 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array (e : _)] = e

last' = mkBuiltin "last" 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = last elements

rest = mkBuiltin "rest" 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = O.Array $ tail elements

push = mkBuiltin "push" 2 [[Just O.ArrayType], [Nothing]] (return . impl)
    where
        impl [O.Array elements, o] = O.Array $ elements ++ [o]

puts = mkBuiltin "puts" (-1) [[Nothing]] impl
    where
        impl os = mapM_ (putStrLn . O.inspect) os >> return O.Null
