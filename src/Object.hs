module Object where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.IORef (IORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Nodes as N
import Text.Printf (printf)

data Environment = Env (Map.Map String Object) (Maybe EnvironmentRef)

type ProgramState = StateT EnvironmentRef (ExceptT String IO)

insert :: String -> Object -> Environment -> Environment
insert key value (Env e o) = Env (Map.insert key value e) o

(!?) :: Environment -> String -> IO (Maybe Object)
Env e o !? s = case e Map.!? s of
    just@(Just _) -> return just
    _ -> case o of
        Just (EnvRef ref) -> readIORef ref >>= (!? s)
        _ -> return Nothing

newtype EnvironmentRef = EnvRef {unRef :: IORef Environment}

instance Eq EnvironmentRef where
    _ == _ = True

instance Ord EnvironmentRef where
    compare l r = EQ

newtype BuiltinFunc = BuiltinFunc ([Object] -> ProgramState Object)

instance Eq BuiltinFunc where
    _ == _ = True

instance Ord BuiltinFunc where
    compare l r = EQ

data ObjectType
    = IntegerType
    | BooleanType
    | StringType
    | NullType
    | ReturnType
    | FunctionType
    | BuiltinType
    | ArrayType
    | HashType
    deriving (Show, Eq, Ord)

data Object
    = Int Integer
    | Bool Bool
    | String String
    | Null
    | Return Object
    | Function [N.Identifier] N.Block EnvironmentRef
    | Builtin String Int [[ObjectType]] BuiltinFunc
    | Array [Object]
    | Hash (Map.Map Object Object)
    deriving (Eq, Ord)

instance Show Object where
    show (Int v) = show v
    show (Bool v) = show v
    show (String v) = v
    show Null = "null"
    show (Return o) = inspect o
    show (Function params body _) = printf "fn(%s) {\n%s\n}" (intercalate ", " $ map show params) (show body)
    show (Builtin {}) = "builtin function"
    show (Array elements) = printf "[%s]" (intercalate ", " $ map inspect elements)
    show (Hash map) =
        printf "{%s}"
            . intercalate ", "
            . Map.foldMapWithKey (\k v -> [printf "%s:%s" (show k) (inspect v)])
            $ map

class IsObject a where
    inspect :: a -> String
    otype :: a -> ObjectType
    keyable :: a -> Bool

instance IsObject Object where
    inspect = show

    otype (Int _) = IntegerType
    otype (Bool _) = BooleanType
    otype (String _) = StringType
    otype Null = NullType
    otype (Return _) = ReturnType
    otype (Function {}) = FunctionType
    otype (Builtin {}) = BuiltinType
    otype (Array {}) = ArrayType
    otype (Hash {}) = HashType

    keyable (Int _) = True
    keyable (String _) = True
    keyable (Bool _) = True
    keyable _ = False
