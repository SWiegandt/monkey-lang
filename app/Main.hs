module Main where

import Control.Monad (void)
import Control.Monad.Error.Class (handleError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (runStateT))
import qualified Evaluator as E
import qualified Lexer as L
import qualified Object as O
import qualified Parser as P
import System.Environment (getArgs)
import qualified Util as U

main :: IO ()
main = do
    args <- getArgs
    env <- U.createEnv
    U.loadStdLib env
    case args of
        [file] -> runFile env file
        _ -> putStrLn "Usage: monkey [FILE]"

runFile :: O.EnvironmentRef -> String -> IO ()
runFile env file = do
    contents <- readFile file
    void . runExceptT . handleError logError . runFile $ contents
    where
        runFile = void . (`runStateT` env) . E.eval . fst . P.runParser . L.runLexer
        logError err = liftIO $ putStrLn ("Error: " ++ err)
