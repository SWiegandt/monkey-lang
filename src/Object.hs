module Object where

data ObjectType = IntegerType | BooleanType | NullType | ReturnType deriving (Show, Eq)

data Object = OInt Integer | OBool Bool | ONull | OReturn Object deriving (Show, Eq)

class IsObject a where
    inspect :: a -> String
    otype :: a -> ObjectType

instance IsObject Object where
    inspect (OInt v) = show v
    inspect (OBool v) = show v
    inspect ONull = "null"
    inspect (OReturn o) = inspect o

    otype (OInt _) = IntegerType
    otype (OBool _) = BooleanType
    otype ONull = NullType
    otype (OReturn _) = ReturnType
