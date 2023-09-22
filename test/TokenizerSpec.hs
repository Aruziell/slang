module TokenizerSpec (spec) where

import Test.Hspec

import Tokenizer
import qualified Token as T

import EitherExpectation (shouldBeRight)


spec :: Spec
spec = do
    it "empty" $ do
        tokenize "" `shouldBeRight` []

    it "skips spaces" $ do
        tokenize "   " `shouldBeRight` []

    it "single-digit integer" $ do
        tokenize "1" `shouldBeRight` [T.Token (T.Integer 1) (T.Location 0 0)]

    it "multiple-digit integer" $ do
        tokenize "1000" `shouldBeRight` [T.Token (T.Integer 1000) (T.Location 0 0)]

    it "plus operator" $ do
        tokenize "+" `shouldBeRight` [T.Token T.Plus (T.Location 0 0)]

    it "integer addition" $ do
        tokenize "1 + 2" `shouldBeRight`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token T.Plus (T.Location 0 2)
            , T.Token (T.Integer 2) (T.Location 0 4)
            ]

    it "prefix integer addition" $ do
        tokenize "+ 1 2" `shouldBeRight`
            [ T.Token T.Plus (T.Location 0 0)
            , T.Token (T.Integer 1) (T.Location 0 2)
            , T.Token (T.Integer 2) (T.Location 0 4)
            ]

    it "postfix integer addition" $ do
        tokenize "1 2 +" `shouldBeRight`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token (T.Integer 2) (T.Location 0 2)
            , T.Token T.Plus (T.Location 0 4)
            ]

    it "integer column location" $ do
        tokenize "1 1  1   1"
        `shouldBeRight`
            [ T.Token (T.Integer 1) (T.Location 0 0)
            , T.Token (T.Integer 1) (T.Location 0 2)
            , T.Token (T.Integer 1) (T.Location 0 5)
            , T.Token (T.Integer 1) (T.Location 0 9)
            ]

    illegalCharacter '~'
    illegalCharacter '!'
    illegalCharacter '%'


illegalCharacter :: Char -> SpecWith (Arg Expectation)
illegalCharacter c = it ("yields IllegalCharacter " ++ [c]) $
    tokenize [c] `shouldBe` Left (IllegalCharacter c)
