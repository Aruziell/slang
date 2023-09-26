module CodeGeneratorSpec (spec) where

import Test.Hspec

import CodeGenerator
import Shorthand.Syntax


spec :: Spec
spec = do
    describe "expression" $ do
        it "single-digit" $ do
            expression $ _int 1
            `shouldBe` ["i32.const 1"]

        it "multi-digit integer" $ do
            expression $ _int 1000
            `shouldBe` ["i32.const 1000"]

        it "integer addition" $ do
            expression $
                _int 1 `_plus` _int 2
            `shouldBe`
                [ "i32.const 1"
                , "i32.const 2"
                , "i32.add"
                ]

        it "three integer addition" $ do
            expression $
                _int 1 `_plus` (_int 2 `_plus` _int 3)
            `shouldBe`
                [ "i32.const 1"
                , "i32.const 2"
                , "i32.add"
                , "i32.const 3"
                , "i32.add"
                ]

        it "six integer addition" $ do
            expression $
                _int 1
                    `_plus` _int 2
                    `_plus` _int 3
                    `_plus` _int 4
                    `_plus` _int 5
                    `_plus` _int 6
            `shouldBe`
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
