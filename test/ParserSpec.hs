module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Syntax as S
import qualified Token as T

import EitherExpectation


spec :: Spec
spec = do
    it "integer" $ do
        parse [T.IntegerToken 1] `shouldBeRight` S.Program (S.IntegerLiteral 1)

    it "integer addition" $ do
        parse [T.IntegerToken 1, T.PlusToken, T.IntegerToken 2]
            `shouldBeRight` S.Program (
                S.PlusOperator (S.IntegerLiteral 1) (S.IntegerLiteral 2)
            )

    it "three integer addition" $ do
        parse
            [ T.IntegerToken 1
            , T.PlusToken
            , T.IntegerToken 2
            , T.PlusToken
            , T.IntegerToken 3
            ]
        `shouldBeRight` S.Program (
            S.PlusOperator (S.IntegerLiteral 1) (
                S.PlusOperator (S.IntegerLiteral 2) (S.IntegerLiteral 3)
            )
        )

    it "missing left add operand" $ do
        parse [T.PlusToken, T.IntegerToken 1]
            `shouldBe` Left IncompleteExpression

    it "missing right add operand" $ do
        parse [T.IntegerToken 1, T.PlusToken]
            `shouldBe` Left IncompleteExpression

    it "missing both add operands" $ do
        parse [T.PlusToken] `shouldBe` Left IncompleteExpression
