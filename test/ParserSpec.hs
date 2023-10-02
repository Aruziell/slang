module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Location as L
import qualified Shorthand.Syntax as S
import qualified Shorthand.Token as T
import qualified Syntax as S
import qualified Token as T

import EitherExpectation


spec :: Spec
spec = do
    describe "literals" $ do
      
        it "integer" $
            parseExpression [T._int 1]
                `shouldBeRight` (S._int 1, [])

    describe "operators" $ do
      
        it "integer addition" $
            parseExpression
                [ T._int 1
                , T._plus
                , T._int 2
                ]
            `shouldBeRight`
                ((S._int 1 `S._plus` S._int 2), [])

        it "three integer addition" $
            parseExpression
                [ T._int 1
                , T._plus
                , T._int 2
                , T._plus
                , T._int 3
                ]
            `shouldBeRight`
                (S._int 1 `S._plus` (S._int 2 `S._plus` S._int 3), [])

        it "missing left add operand" $ do
            parseExpression [T._plus, T._int 1]
            `shouldBe` Left IncompleteExpression

        it "missing right add operand" $ do
            parseExpression [T._int 1, T._plus]
            `shouldBe` Left IncompleteExpression
    
        it "missing both add operands" $ do
            parseExpression [T._plus]
            `shouldBe` Left IncompleteExpression

    describe "calls" $ do

        it "call expression" $
            parseExpression [T._id "foo"]
            `shouldBeRight` (S._call "foo" [], [])

        it "call with a parameter" $
            parseExpression
                [ T._id "foo", T._int 1
                ]
            `shouldBeRight` (S._call "foo" [(S._int 1)], [])

        it "call with multiple parameters" $
            parseExpression
                [ T._id "foo", T._int 1, T._int 2, T._int 3 ]
            `shouldBeRight`
                (S._call "foo" [S._int 1, S._int 2, S._int 3], [])

    describe "functions" $ do
      
        it "constant" $
            parseFunction
                [ T._id "foo", T._eq, T._int 1, T._end ]
            `shouldBeRight`
                (S._fn "foo" [] (S._int 1), [])

        it "with a parameter" $
            parseFunction [T._id "foo", T._id "bar", T._eq, T._int 1, T._end]
            `shouldBeRight` (S._fn "foo" [S._arg "bar"] (S._int 1), [])

        it "using its parameter" $
            parseFunction
                [ T._id "foo", T._id "a", T._eq
                , T._id "a", T._plus, T._id "a", T._end
                ]
            `shouldBeRight`
                (S._fn "foo" [S._arg "a"]
                    ((S._call "a" []) `S._plus` (S._call "a" [])), [])

        it "function list" $
            parseFunctionList
                [ T._id "foo", T._eq, T._int 1, T._end
                , T._id "bar", T._eq, T._int 2, T._end
                ]
            `shouldBeRight`
                [ S._fn "foo" [] (S._int 1)
                , S._fn "bar" [] (S._int 2)
                ]

    it "addition of function calls" $
        parseExpression
            [ T._id "foo", T._int 1, T._plus, T._id "bar", T._int 2 ]
        `shouldBeRight`
            (S._call "foo" [S._int 1] `S._plus` S._call "bar" [S._int 2], [])

    it "preserves location" $ do
        parseExpression [T.Token (T.Integer 1) location]
        `shouldBeRight` (S.Expression (S.Literal (S.Integer 1)) location, [])
        where location = L.Location 3 5
