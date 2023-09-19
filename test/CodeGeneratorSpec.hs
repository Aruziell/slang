module CodeGeneratorSpec (spec) where

import Test.Hspec

import CodeGenerator
import Syntax as S


spec :: Spec
spec = do
    it "single-digit program" $ do
        generateWat (S.Program $ S.IntegerLiteral 1)
            `shouldBe` watProgram ["i32.const 1"]

    it "multi-digit integer program" $ do
        generateWat (S.Program $ S.IntegerLiteral 1000)
            `shouldBe` watProgram ["i32.const 1000"]

    it "integer addition program" $ do
        generateWat $ S.Program $
            S.PlusOperator (S.IntegerLiteral 1) (S.IntegerLiteral 2)
        `shouldBe` watProgram
            [ "i32.const 1"
            , "i32.const 2"
            , "i32.add"
            ]

    it "three integer addition" $ do
        generateWat $ S.Program $
            S.PlusOperator (S.IntegerLiteral 1) $
            S.PlusOperator (S.IntegerLiteral 2) (S.IntegerLiteral 3)
        `shouldBe` watProgram
            [ "i32.const 1"
            , "i32.const 2"
            , "i32.add"
            , "i32.const 3"
            , "i32.add"
            ]
    
    it "six integer addition" $ do
        generateWat $ S.Program $
            S.PlusOperator (S.IntegerLiteral 1) $
            S.PlusOperator (S.IntegerLiteral 2) $
            S.PlusOperator (S.IntegerLiteral 3) $
            S.PlusOperator (S.IntegerLiteral 4) $
            S.PlusOperator (S.IntegerLiteral 5) (S.IntegerLiteral 6)
        `shouldBe` watProgram
            [ "i32.const 1"
            , "i32.const 2"
            , "i32.add"
            , "i32.const 3"
            , "i32.add"
            , "i32.const 4"
            , "i32.add"
            , "i32.const 5"
            , "i32.add"
            , "i32.const 6"
            , "i32.add"
            ]


watProgram :: [String] -> String
watProgram content =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    (content >>= ("        " ++) . (++ "\n")) ++
    "    )\n" ++
    ")\n"
