module CodeGeneratorSpec (spec) where

import Test.Hspec

import CodeGenerator
import Shorthand.Syntax


spec :: Spec
spec = do
    describe "expression" $ do
        it "single-digit" $ do
            expression [] (_int 1)
            `shouldBe` ["i32.const 1"]

        it "multi-digit integer" $ do
            expression [] (_int 1000)
            `shouldBe` ["i32.const 1000"]

        it "integer addition" $ do
            expression [] $
                _int 1 `_plus` _int 2
            `shouldBe`
                [ "i32.const 1"
                , "i32.const 2"
                , "i32.add"
                ]

        it "three integer addition" $ do
            expression [] $
                _int 1 `_plus` (_int 2 `_plus` _int 3)
            `shouldBe`
                [ "i32.const 1"
                , "i32.const 2"
                , "i32.add"
                , "i32.const 3"
                , "i32.add"
                ]

        it "six integer addition" $ do
            expression [] $
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

    describe "function" $ do
        it "constant" $
            function (_fn "foo" [] (_int 1))
            `shouldBe`
                [ "(func $foo (result i32)"
                , "    i32.const 1"
                , ")"
                ]

        it "function with arguments" $
            function (_fn "foo" [_arg "bar", _arg "baz"] (_call "foo" []))
            `shouldBe`
                [ "(func $foo (param $bar i32) (param $baz i32) (result i32)"
                , "    call $foo"
                , ")"
                ]

        it "function using its argument" $
            function (_fn "foo" [_arg "bar", _arg "baz"] (_call "bar" []))
            `shouldBe`
                [ "(func $foo (param $bar i32) (param $baz i32) (result i32)"
                , "    local.get $bar"
                , ")"
                ]

        it "function call without arguments" $
            expression [] (_call "foo" [])
            `shouldBe` ["call $foo"]

        it "function call with a single argument" $
            expression [] (_call "foo" [_int 1] )
            `shouldBe` ["i32.const 1", "call $foo"]

        it "function call with arguments" $
            expression []
                (_call "foo" [_int 1, _call "bar" [_int 2]])
            `shouldBe`
                [ "i32.const 1"
                , "i32.const 2"
                , "call $bar"
                , "call $foo"
                ]

    describe "scenarios" $ do
        it "double" $
            function
                (_fn "foo" [_arg "bar"]
                    ((_call "bar" []) `_plus` (_call "bar" [])))
            `shouldBe`
                [ "(func $foo (param $bar i32) (result i32)"
                , "    local.get $bar"
                , "    local.get $bar"
                , "    i32.add"
                , ")"
                ]

        it "add" $
            function
                (_fn "add" [_arg "a", _arg "b"]
                    (_call "a" [] `_plus` _call "b" []))
            `shouldBe`
                [ "(func $add (param $a i32) (param $b i32) (result i32)"
                , "    local.get $a"
                , "    local.get $b"
                , "    i32.add"
                , ")"
                ]
