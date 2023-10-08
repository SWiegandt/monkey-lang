module Monkey.Util where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (StateT (runStateT))
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Monkey.Eval.Evaluator as E
import qualified Monkey.Eval.Object as O
import Monkey.Frontend.Lexer (runLexer)
import Monkey.Frontend.Parser (runParser)
import Paths_monkey_lang

loadStdLib :: O.EnvironmentRef -> IO ()
loadStdLib env = do
    stdLib <- getDataFileName "lib/std.monkey" >>= readFile
    let (p, _) = runParser . runLexer $ stdLib
    void . runExceptT . (`runStateT` env) . E.eval $ p

createEnv :: IO O.EnvironmentRef
createEnv = O.EnvRef <$> newIORef (O.Env Map.empty Nothing)

note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just a) = Right a
