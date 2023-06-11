module Main where

import Lexer (runLexer)
import Parser (Program (Program), runParser)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Tokens as T

repl :: IO ()
repl = do
    putStr ">> " >> hFlush stdout
    line <- getLine
    let (p, log) = runParser . runLexer $ line
    if null log
        then print p
        else mapM_ (printf "\t%s\n") log
    repl

main :: IO ()
main = do
    user <- getEnv "USER"
    printf "Hello %s! This is the Monkey programming language!\n" user
    putStrLn "Feel free to type in commands"
    repl
