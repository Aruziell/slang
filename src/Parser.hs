module Parser (parse) where

import qualified Syntax as S
import qualified Token as T


parse :: [T.Token] -> S.Program
parse = S.Program . parseExpression


parseExpression :: [T.Token] -> S.Expression
parseExpression [T.IntegerToken value] =
    S.IntegerLiteral value
parseExpression tokens@(T.IntegerToken lhs : T.PlusToken : _) =
    parseAdd tokens


parseAdd :: [T.Token] -> S.Expression
parseAdd (T.IntegerToken lhs : T.PlusToken : rest) =
    S.PlusOperator (S.IntegerLiteral lhs) (parseExpression rest)
