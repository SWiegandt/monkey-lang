module Main where

import Control.Monad.State (StateT (runStateT))
import qualified Data.Map as Map
import qualified Evaluator as E
import Lexer (runLexer)
import qualified Object as O
import Parser (runParser)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

repl :: Map.Map String O.Object -> IO ()
repl state = do
    putStr ">> " >> hFlush stdout
    line <- getLine
    let (p, log) = runParser . runLexer $ line
    state <-
        if null log
            then case (`runStateT` state) $ E.eval p of
                Left s -> putStrLn s >> return state
                Right (o, state) -> putStrLn (O.inspect o) >> return state
            else mapM_ (printf "\t%s\n") log >> return state
    repl state

main :: IO ()
main = do
    user <- getEnv "USER"
    printf "Hello %s! This is the Monkey programming language!\n" user
    putStrLn "Feel free to type in commands"
    repl Map.empty
