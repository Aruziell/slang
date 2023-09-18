module TokenizerSpec (spec) where

import Test.Hspec

import Tokenizer
import qualified Token as T


spec :: Spec
spec = do
    it "single-digit integer" $ do
        tokenize "1" `shouldBe` [T.IntegerToken 1]

    it "multiple-digit integer" $ do
        tokenize "1000" `shouldBe` [T.IntegerToken 1000]

    it "plus operator" $ do
        tokenize "+" `shouldBe` [T.PlusToken]

    it "integer addition" $ do
        tokenize "1 + 2" `shouldBe`
            [T.IntegerToken 1, T.PlusToken, T.IntegerToken 2]

    it "prefix integer addition" $ do
        tokenize "+ 1 2" `shouldBe`
            [T.PlusToken, T.IntegerToken 1, T.IntegerToken 2]

    it "postfix integer addition" $ do
        tokenize "1 2 +" `shouldBe`
            [T.IntegerToken 1, T.IntegerToken 2, T.PlusToken]
