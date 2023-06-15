module Object where

import Data.IORef (IORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Nodes as N
import Text.Printf (printf)

data Environment = Env (Map.Map String Object) (Maybe (IORef Environment))

insert :: String -> Object -> Environment -> Environment
insert key value (Env e o) = Env (Map.insert key value e) o

(!?) :: Environment -> String -> IO (Maybe Object)
Env e o !? s = case e Map.!? s of
    just@(Just _) -> return just
    _ -> case o of
        Just ref -> readIORef ref >>= (!? s)
        _ -> return Nothing

data ObjectType = IntegerType | BooleanType | NullType | ReturnType | FunctionType deriving (Show, Eq)

data Object
    = OInt Integer
    | OBool Bool
    | ONull
    | OReturn Object
    | OFunction [N.Identifier] N.Block (IORef Environment)

instance Eq Object where
    OInt n == OInt m = n == m
    OBool p == OBool q = p == q
    ONull == ONull = True
    OReturn l == OReturn r = l == r
    OFunction pl bl _ == OFunction pr br _ = pl == pr && bl == br
    _ == _ = False

class IsObject a where
    inspect :: a -> String
    otype :: a -> ObjectType

instance IsObject Object where
    inspect (OInt v) = show v
    inspect (OBool v) = show v
    inspect ONull = "null"
    inspect (OReturn o) = inspect o
    inspect (OFunction params body _) = printf "fn(%s) {\n%s\n}" (intercalate ", " $ map show params) (show body)

    otype (OInt _) = IntegerType
    otype (OBool _) = BooleanType
    otype ONull = NullType
    otype (OReturn _) = ReturnType
    otype (OFunction {}) = FunctionType
