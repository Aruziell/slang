module ToolchainSpec where

import Test.Hspec

import Toolchain
import Tokenizer (tokenize)
import Parser (parse)
import CodeGenerator (program)
import EitherExpectation


spec :: Spec
spec = do
    it "toolchain" $ do
        let toolchain = createToolchain tokenize parse program

        toolchain "main = 1\n"
        `shouldBeRight` unlines
            [ "(module"
            , "    (func (export \"_start\") (result i32)"
            , "        i32.const 1"
            , "    )"
            , ")"
            ]
