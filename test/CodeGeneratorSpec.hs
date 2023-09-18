module CodeGeneratorSpec (spec) where

import Test.Hspec

import CodeGenerator
import Syntax as S


spec :: Spec
spec = do
    it "generates single-digit program" $ do
        generateWat (S.Program $ S.IntegerLiteral 1)
            `shouldBe` watProgram "i32.const 1"
            
    it "generates multi-digit integer program" $ do
        generateWat (S.Program $ S.IntegerLiteral 1000)
            `shouldBe` watProgram "i32.const 1000"


watProgram :: String -> String
watProgram content =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    "        " ++ content ++ "\n" ++
    "    )\n" ++
    ")\n"
