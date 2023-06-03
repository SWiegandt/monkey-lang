module Main where

import Lexer
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

repl :: IO ()
repl = do
    putStr "> " >> hFlush stdout
    line <- getLine
    mapM_ print . filter (\t -> ttype t /= EOF) $ runLexer line
    repl

main :: IO ()
main = do
    user <- getEnv "USER"
    printf "Hello %s! This is the Monkey programming language!\n" user
    putStrLn "Feel free to type in commands"
    repl
