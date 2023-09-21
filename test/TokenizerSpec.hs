module TokenizerSpec (spec) where

import Test.Hspec

import Tokenizer
import qualified Token as T


spec :: Spec
spec = do
    it "empty" $ do
        tokenize "" `shouldBe` []

    it "skips spaces" $ do
        tokenize "   " `shouldBe` []

    it "single-digit integer" $ do
        tokenize "1" `shouldBe` [T.Token (T.Integer 1) (T.Location 0 0)]

    it "multiple-digit integer" $ do
        tokenize "1000" `shouldBe` [T.Token (T.Integer 1000) (T.Location 0 0)]

    it "plus operator" $ do
        tokenize "+" `shouldBe` [T.Token T.Plus (T.Location 0 0)]

    it "integer addition" $ do
        tokenize "1 + 2" `shouldBe`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token T.Plus (T.Location 0 2)
            , T.Token (T.Integer 2) (T.Location 0 4)
            ]

    it "prefix integer addition" $ do
        tokenize "+ 1 2" `shouldBe`
            [ T.Token T.Plus (T.Location 0 0)
            , T.Token (T.Integer 1) (T.Location 0 2)
            , T.Token (T.Integer 2) (T.Location 0 4)
            ]

    it "postfix integer addition" $ do
        tokenize "1 2 +" `shouldBe`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token (T.Integer 2) (T.Location 0 2)
            , T.Token T.Plus (T.Location 0 4)
            ]

    it "integer column location" $ do
        tokenize "1 1  1   1"
        `shouldBe`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token (T.Integer 1) (T.Location 0 2)
            , T.Token (T.Integer 1) (T.Location 0 5)
            , T.Token (T.Integer 1) (T.Location 0 9)
            ]
