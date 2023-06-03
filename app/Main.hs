module Main where

import Control.Monad.State (evalState)
import Lexer

main :: IO ()
main = do
    let str = ";=)"
    print $ runLexer str
