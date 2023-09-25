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
parseExpression [T.Token (T.Integer value) loc] =
    Right (S.Expression (S.IntegerLiteral value) loc)
parseExpression tokens@(T.Token (T.Integer _) _ : T.Token T.Plus _ : T.Token (T.Integer _) _ : _) =
    parseAdd tokens
parseExpression _ =
    Left IncompleteExpression


parseAdd :: Parser S.Expression
parseAdd (T.Token (T.Integer lhs) lLoc : T.Token T.Plus opLoc : rest@(T.Token (T.Integer _) _ : _)) = do
    let left = S.Expression (S.IntegerLiteral lhs) lLoc
    right <- parseExpression rest
    Right $ S.Expression (S.PlusOperator left right) opLoc
parseAdd _ =
    Left IncompleteExpression
