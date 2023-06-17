module Object where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.IORef (IORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Nodes as N
import Text.Printf (printf)

data Environment = Env (Map.Map String Object) (Maybe (IORef Environment))

type ProgramState = StateT (IORef Environment) (ExceptT String IO)

insert :: String -> Object -> Environment -> Environment
insert key value (Env e o) = Env (Map.insert key value e) o

(!?) :: Environment -> String -> IO (Maybe Object)
Env e o !? s = case e Map.!? s of
    just@(Just _) -> return just
    _ -> case o of
        Just ref -> readIORef ref >>= (!? s)
        _ -> return Nothing

data ObjectType
    = IntegerType
    | BooleanType
    | StringType
    | NullType
    | ReturnType
    | FunctionType
    | BuiltinType
    | ArrayType
    deriving (Show, Eq)

data Object
    = Int Integer
    | Bool Bool
    | String String
    | Null
    | Return Object
    | Function [N.Identifier] N.Block (IORef Environment)
    | Builtin Int [[Maybe ObjectType]] ([Object] -> IO Object)
    | Array [Object]

instance Eq Object where
    Int n == Int m = n == m
    Bool p == Bool q = p == q
    String l == String r = l == r
    Null == Null = True
    Return l == Return r = l == r
    Function pl bl _ == Function pr br _ = pl == pr && bl == br
    _ == _ = False

class IsObject a where
    inspect :: a -> String
    otype :: a -> ObjectType

instance IsObject Object where
    inspect (Int v) = show v
    inspect (Bool v) = show v
    inspect (String v) = v
    inspect Null = "null"
    inspect (Return o) = inspect o
    inspect (Function params body _) = printf "fn(%s) {\n%s\n}" (intercalate ", " $ map show params) (show body)
    inspect (Builtin {}) = "builtin function"
    inspect (Array elements) = printf "[%s]" $ intercalate ", " (map inspect elements)

    otype (Int _) = IntegerType
    otype (Bool _) = BooleanType
    otype (String _) = StringType
    otype Null = NullType
    otype (Return _) = ReturnType
    otype (Function {}) = FunctionType
    otype (Builtin {}) = BuiltinType
    otype (Array {}) = ArrayType
