module Monkey.Backend.CodeSpec where

import Control.Monad (forM_)
import Monkey.Backend.Code (Instructions (Instructions), Opcode (op), make, opConstant)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "MakeCode" testMakeCode

testMakeCode = do
    let tests = [(opConstant, [65534], Instructions [op opConstant, 255, 254])]
    forM_ tests $ \(code, operands, expectation) -> do
        it "should make correct op codes" $ make code operands `shouldBe` expectation
