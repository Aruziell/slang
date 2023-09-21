module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Syntax as S
import qualified Token as T

import EitherExpectation


spec :: Spec
spec = do
    it "integer" $ do
        parse [T.Token (T.Integer 1) _location]
            `shouldBeRight` S.Program (S.IntegerLiteral 1)

    it "integer addition" $ do
        parse
            [ T.Token (T.Integer 1) _location
            , T.Token T.Plus _location
            , T.Token (T.Integer 2) _location
            ]
        `shouldBeRight` S.Program (
            S.PlusOperator (S.IntegerLiteral 1) (S.IntegerLiteral 2)
        )

    it "three integer addition" $ do
        parse
            [ T.Token (T.Integer 1) _location
            , T.Token T.Plus _location
            , T.Token (T.Integer 2) _location
            , T.Token T.Plus _location
            , T.Token (T.Integer 3) _location
            ]
        `shouldBeRight` S.Program (
            S.PlusOperator (S.IntegerLiteral 1) (
                S.PlusOperator (S.IntegerLiteral 2) (S.IntegerLiteral 3)
            )
        )

    it "missing left add operand" $ do
        parse [T.Token T.Plus _location, T.Token (T.Integer 1) _location]
            `shouldBe` Left IncompleteExpression

    it "missing right add operand" $ do
        parse [T.Token (T.Integer 1) _location, T.Token T.Plus _location]
            `shouldBe` Left IncompleteExpression

    it "missing both add operands" $ do
        parse [T.Token T.Plus _location] `shouldBe` Left IncompleteExpression


_location :: T.Location 
_location = T.Location 0 0
