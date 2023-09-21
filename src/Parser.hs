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
parseExpression [T.Token (T.Integer value) _] =
    Right (S.IntegerLiteral value)
parseExpression tokens@(T.Token (T.Integer _) _ : T.Token T.Plus _ : T.Token (T.Integer _) _ : _) =
    parseAdd tokens
parseExpression _ =
    Left IncompleteExpression


parseAdd :: Parser S.Expression
parseAdd (T.Token (T.Integer lhs) _ : T.Token T.Plus _ : rest@(T.Token (T.Integer _) _ : _)) =
    S.PlusOperator (S.IntegerLiteral lhs) <$> (parseExpression rest)
parseAdd _ =
    Left IncompleteExpression
