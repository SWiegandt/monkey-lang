module Monkey.Backend.Code where

import Control.Arrow (Arrow (second))
import Control.Monad.State (MonadTrans (lift), StateT, evalStateT, get, gets, modify, put)
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Monkey.Util (note)
import Text.Printf (printf)
import Prelude hiding (lookup, read)

newtype Opcode = Opcode {op :: Word8} deriving (Eq)

newtype Instructions = Instructions {words :: [Word8]} deriving (Eq)

instance Show Instructions where
    show = read

data Definition = Definition String [Int]

opConstant = Opcode 0

definition :: Opcode -> Maybe Definition
definition op
    | op == opConstant = Just $ Definition "OpConstant" [2]
    | otherwise = Nothing

lookup :: Word8 -> Either String Definition
lookup op = note (printf "opcode %d undefined" op) $ definition (Opcode op)

make :: Opcode -> [Int] -> Instructions
make opcode operands = Instructions . fromMaybe [] $ do
    Definition _ widths <- definition opcode
    return $ op opcode : concat (zipWith makeOperand widths operands)

makeOperand :: Int -> Int -> [Word8]
makeOperand 2 operand = unpack . encode $ (fromIntegral operand :: Word16)
makeOperand width _ = error $ printf "can't make operand with width %d" width

type ReadState a = StateT ([Word8], Int) (Either String) a

read :: Instructions -> String
read (Instructions words) = either id id . (`evalStateT` (words, 0)) $ loop
    where
        loop :: ReadState String
        loop = do
            (words, offset) <- get
            case words of
                [] -> return ""
                (op : operands) -> do
                    Definition desc widths <- lift . (lookup . head) $ words
                    instructions <- concat <$> mapM (readInstruction desc operands) widths
                    modify $ second (+ 1)
                    (instructions ++) <$> loop

readInstruction :: String -> [Word8] -> Int -> ReadState String
readInstruction desc operands width = do
    let (operand, rest) = splitAt width operands
    offset <- gets snd
    put (rest, offset + width)
    return $ printf "%04d %s %d\n" offset desc (decodeOperand width operand)

decodeOperand :: Int -> [Word8] -> Int
decodeOperand 2 operand = fromIntegral (decode . pack $ operand :: Word16)
decodeOperand width _ = error $ printf "can't decode operand with width %d" width
