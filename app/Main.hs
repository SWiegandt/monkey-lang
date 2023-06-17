module Main where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT (runStateT))
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Evaluator as E
import Lexer (runLexer)
import qualified Object as O
import Parser (runParser)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

loadStdLib :: O.EnvironmentRef -> IO ()
loadStdLib env = do
    stdLib <- readFile "/Users/sebastianwiegandt/git/monkey-lang/lib/std.monkey"
    let (p, _) = runParser . runLexer $ stdLib
    void . runExceptT . (`runStateT` env) . E.eval $ p

repl :: O.EnvironmentRef -> IO ()
repl env = do
    putStr ">> " >> hFlush stdout
    line <- getLine
    let (p, log) = runParser . runLexer $ line
    if null log
        then do
            result <- runExceptT . (`runStateT` env) . E.eval $ p
            case result of
                Left s -> putStrLn s
                Right (O.Null, _) -> return ()
                Right (o, env) -> putStrLn (O.inspect o)
        else mapM_ (printf "\t%s\n") log
    repl env

main :: IO ()
main = do
    user <- getEnv "USER"
    printf "Hello %s! This is the Monkey programming language!\n" user
    putStrLn "Feel free to type in commands"
    env <- O.EnvRef <$> newIORef (O.Env Map.empty Nothing)
    loadStdLib env
    repl env
