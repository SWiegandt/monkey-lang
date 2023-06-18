module Util where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (StateT (runStateT))
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Evaluator as E
import Lexer (runLexer)
import qualified Object as O
import Parser (runParser)
import Paths_monkey_lang

loadStdLib :: O.EnvironmentRef -> IO ()
loadStdLib env = do
    stdLib <- getDataFileName "lib/std.monkey" >>= readFile
    let (p, _) = runParser . runLexer $ stdLib
    void . runExceptT . (`runStateT` env) . E.eval $ p

createEnv :: IO O.EnvironmentRef
createEnv = O.EnvRef <$> newIORef (O.Env Map.empty Nothing)
