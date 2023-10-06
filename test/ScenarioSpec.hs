module ScenarioSpec (spec) where

import Data.Either (isRight)
import Test.Hspec

import Toolchain


spec :: Spec
spec = do
    let toolchain = defaultRunToolchain
    let shouldOutput = createShouldOutput toolchain

    it "constant" $
        "main = 1\n" `shouldOutput` "1\n"

    it "addition" $
        "main = 1 + 2\n" `shouldOutput` "3\n"

    it "triangular number recursive" $
        unlines
            [ "main = tri 10"
            , "tri n = trii 0 n"
            , "trii i, n = when i"
            , "    n then n"
            , "    else i + trii (i+1) n"
            ]
        `shouldOutput` "55\n"

    it "fibonacci tail recursive" $
        unlines
            [ "main = fib 10"
            , "fib n = fibi 0 n 0 1"
            , "fibi i, n, a, b = when i"
            , "    n then a"
            , "    else fibi (i+1) n b (a+b)"
            ]
        `shouldOutput` "55\n"

    it "factorial tail recursive" $
        unlines
            [ "main = fac 10"
            , "fac n = faci 1 n"
            , "faci i, n = when i"
            , "    n then n"
            , "    else mul i (faci (i+1) n)"
            , "mul a, b = muli 0 0 a b"
            , "muli i, r, a, b = when i"
            , "    a then r"
            , "    else muli (i+1) (r+b) a b"
            ]
        `shouldOutput` "3628800\n"


createShouldOutput :: RunToolchain -> String -> String -> Expectation
createShouldOutput toolchain input expect = do
    result <- toolchain input
    result `shouldSatisfy` isRight
    Right (stdout, _) <- toolchain input
    stdout `shouldBe` expect
