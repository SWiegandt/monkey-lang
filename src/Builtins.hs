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
checkArgumentLength f@(O.Builtin n _ _) args
    | n == -1 || n == length args = liftEither $ Right f
    | otherwise = throwError $ printf "wrong number of arguments. got=%s, want=%s" (show $ length args) (show n)

checkArgumentTypes :: O.Object -> [O.Object] -> O.ProgramState O.Object
checkArgumentTypes f@(O.Builtin _ types _) args
    | and $ zipWith (\tpe arg -> Nothing `elem` tpe || Just (O.otype arg) `elem` tpe) types args = liftEither $ Right f
    | otherwise = throwError $ printf "argument to `len` not supported, got %s" (show . O.otype $ head args)

runBuiltin :: O.Object -> [O.Object] -> O.ProgramState O.Object
runBuiltin f@(O.Builtin _ _ impl) args = do
    checkArgumentLength f args
    checkArgumentTypes f args
    liftIO $ impl args

len = O.Builtin 1 [[Just O.StringType, Just O.ArrayType]] (return . impl)
    where
        impl [O.String str] = O.Int $ genericLength str
        impl [O.Array arr] = O.Int $ genericLength arr

first = O.Builtin 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array (e : _)] = e

last' = O.Builtin 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = last elements

rest = O.Builtin 1 [[Just O.ArrayType]] (return . impl)
    where
        impl [O.Array []] = O.Null
        impl [O.Array elements] = O.Array $ tail elements

push = O.Builtin 2 [[Just O.ArrayType], [Nothing]] (return . impl)
    where
        impl [O.Array elements, o] = O.Array $ elements ++ [o]

puts = O.Builtin (-1) [[Nothing]] impl
    where
        impl os = mapM_ (putStrLn . O.inspect) os >> return O.Null
