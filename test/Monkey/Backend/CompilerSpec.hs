module Monkey.Backend.CompilerSpec where

import Control.Monad (forM_)
import Data.Word (Word8)
import Monkey.Backend.Code (Instructions (Instructions), make, opConstant)
import Monkey.Backend.Compiler (Bytecode (Bytecode), compile)
import Monkey.Eval.Object (Object (Int))
import Monkey.Frontend.Lexer (runLexer)
import Monkey.Frontend.Parser (runParser)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import Text.Printf (printf)

type CompilerTestCase = (String, [Word8], [Instructions])

data CompilerConstant = IntConstant Integer deriving (Show)

spec :: Spec
spec = do
    describe "IntegerArithmetic" testIntegerArithmetic

testIntegerArithmetic = do
    let tests = [("1 + 2", [IntConstant 1, IntConstant 2], [make opConstant [0], make opConstant [1]])]
    forM_ tests $ \(input, constants, instructions) -> do
        it "should compile arithmetic statements" $ do
            let (program, _) = runParser . runLexer $ input
            case compile 0 program of
                Right (Bytecode i o) -> do
                    testInstructions instructions i
                    testConstants constants o
                Left error -> expectationFailure error

testInstructions :: [Instructions] -> Instructions -> Expectation
testInstructions expected actual = actual `shouldBe` Instructions (concatMap (\(Instructions words) -> words) expected)

testConstants :: [CompilerConstant] -> [Object] -> Expectation
testConstants expected actual = forM_ (zip expected actual) $ \(constant, object) -> do
    case (constant, object) of
        (IntConstant c, Int o) -> o `shouldBe` c
        _ -> expectationFailure $ printf "%s doesn't match expected %s" (show object) (show constant)
