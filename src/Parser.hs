module Parser (ParseError(..), parse) where

import qualified Syntax as S
import qualified Token as T


type Parser a = [T.Token] -> Either ParseError a


data ParseError
    = IncompleteExpression
    deriving (Eq, Show)


parse :: Parser S.Program
parse tokens = S.Program <$> parseExpression tokens


parseExpression :: Parser S.Expression
parseExpression [T.IntegerToken value] =
    Right (S.IntegerLiteral value)
parseExpression tokens@(T.IntegerToken _ : T.PlusToken : T.IntegerToken _ : _) =
    parseAdd tokens
parseExpression _ =
    Left IncompleteExpression


parseAdd :: Parser S.Expression
parseAdd (T.IntegerToken lhs : T.PlusToken : rest@(T.IntegerToken _ : _)) =
    S.PlusOperator (S.IntegerLiteral lhs) <$> (parseExpression rest)
parseAdd _ =
    Left IncompleteExpression
