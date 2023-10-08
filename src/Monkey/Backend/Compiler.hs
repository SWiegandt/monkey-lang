module Monkey.Backend.Compiler where

import Monkey.AST.Nodes (Node)
import Monkey.Backend.Code (Instructions (Instructions))
import Monkey.Eval.Object (Object)

data Bytecode = Bytecode Instructions [Object]

compile :: (Node a) => a -> Bytecode
compile node = Bytecode (Instructions []) []
