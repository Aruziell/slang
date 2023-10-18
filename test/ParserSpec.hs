module ParserSpec (spec) where

import Data.Either
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
    describe "literal" $ do
      
        it "integer" $
            parseExpression [T._int 1]
                `shouldBeRight` (S._int 1, [])

    describe "operator" $ do
      
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
                (S._int 1 `S._plus` S._int 2 `S._plus` S._int 3, [])

        it "integer subtraction" $
            parseExpression
                [ T._int 1
                , T._minus
                , T._int 2
                ]
            `shouldBeRight`
                ((S._int 1 `S._minus` S._int 2), [])

        it "mixed integer addition and subtraction" $
            parseExpression
                [ T._int 1
                , T._minus
                , T._int 2
                , T._plus
                , T._int 3
                , T._minus
                , T._int 4
                ]
            `shouldBeRight`
                ( S._int 1 `S._minus` S._int 2
                  `S._plus` S._int 3 `S._minus` S._int 4
                , [])

        describe "logical" $ do

            it "greater than" $
                parseExpression [T._int 1, T._gt, T._int 2]
                `shouldBeRight` (S._gt (S._int 1) (S._int 2), [])

            it "greater than additives" $
                parseExpression
                    [ T._int 1, T._plus, T._int 2
                    , T._gt
                    , T._int 3, T._minus, T._int 4
                    ]
                `shouldBeRight`
                    ( (S._int 1 `S._plus` S._int 2)
                        `S._gt`
                        (S._int 3 `S._minus` S._int 4)
                    , []
                    )

        it "missing left add operand" $ do
            parseExpression [T._plus, T._int 1]
            `shouldSatisfy` isLeft

        it "missing right add operand" $ do
            parseExpression [T._int 1, T._plus]
            `shouldSatisfy` isLeft
    
        it "missing both add operands" $ do
            parseExpression [T._plus]
            `shouldSatisfy` isLeft

    describe "parentheses" $ do

        it "constant" $
            parseExpression [T._pLeft, T._int 1, T._pRight]
            `shouldBeRight` (S._paren (S._int 1), [])

        it "addition" $
            parseExpression [T._pLeft, T._int 1, T._plus, T._int 2, T._pRight]
            `shouldBeRight` (S._paren (S._int 1 `S._plus` S._int 2), [])

        it "call argument" $
            parseExpression [T._id "foo", T._pLeft, T._int 1, T._pRight]
            `shouldBeRight` (S._call "foo" [(S._paren (S._int 1))], [])

        it "missing opening parenthesis" $
            parseExpression [T._int 1, T._pRight]
            `shouldBeRight` (S._int 1, [T._pRight])

        it "missing closing parenthesis" $
            parseExpression [T._pLeft, T._int 1]
            `shouldSatisfy` isLeft

    describe "call" $ do

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

    describe "function" $ do
      
        it "constant" $
            parseFunction
                [ T._id "foo", T._eq, T._int 1 ]
            `shouldBeRight`
                (S._fn "foo" [] (S._int 1), [])

        it "with a parameter" $
            parseFunction [T._id "foo", T._id "bar", T._eq, T._int 1 ]
            `shouldBeRight` (S._fn "foo" [S._arg "bar"] (S._int 1), [])

        it "using its parameter" $
            parseFunction
                [ T._id "foo", T._id "a", T._eq
                , T._id "a", T._plus, T._id "a"
                ]
            `shouldBeRight`
                (S._fn "foo" [S._arg "a"]
                    ((S._call "a" []) `S._plus` (S._call "a" [])), [])

        it "with multiple parameters" $
            parseFunction
                [ T._id "foo", T._id "a", T._sep, T._id "b", T._sep, T._id "c"
                , T._eq, T._int 1
                ]
            `shouldBeRight`
                ( S._fn "foo" [S._arg "a", S._arg "b", S._arg "c"] (S._int 1)
                , []
                )

        it "list" $
            parseFunctionList
                [ T._id "foo", T._eq, T._int 1
                , T._id "bar", T._eq, T._int 2
                ]
            `shouldBeRight`
                [ S._fn "foo" [] (S._int 1)
                , S._fn "bar" [] (S._int 2)
                ]

        it "list skips newlines" $
            parseFunctionList
                [ T._newline
                , T._id "foo", T._eq, T._int 1, T._newline
                , T._newline
                , T._newline
                ]
            `shouldBeRight`
                [ S._fn "foo" [] (S._int 1)
                ]

    describe "condition" $

        it "two cases when-then" $
            parseExpression
                [ T._when, T._int 1
                , T._begin
                    , T._int 2, T._then, T._int 3, T._newline
                    , T._int 4, T._then, T._int 5, T._newline
                    , T._else, T._int 6
                , T._end
                ]
            `shouldBeRight`
                ( S._when (S._int 1)
                    [ (S._int 2, S._int 3)
                    , (S._int 4, S._int 5)
                    ]
                    (S._int 6)
                , []
                )

    it "addition of function calls" $
        parseExpression
            [ T._id "foo", T._int 1, T._plus, T._id "bar", T._int 2 ]
        `shouldBeRight`
            (S._call "foo" [S._int 1] `S._plus` S._call "bar" [S._int 2], [])

    it "preserves location" $ do
        parseExpression [T.Token (T.Integer 1) location]
        `shouldBeRight` (S.Expression (S.Literal (S.Integer 1)) location, [])
        where location = L.Location 3 5
