module Parser (ParseError(..), parse, parseExpression) where

import qualified Syntax as S
import qualified Token as T


type Parser a = [T.Token] -> Either ParseError a


data ParseError
    = IncompleteDefinition
    | IncompleteExpression
    deriving (Eq, Show)


parse :: Parser S.Program
parse tokens = S.Program <$> parseDefinition tokens


parseDefinition :: Parser S.Definition
parseDefinition (T.Token (T.Identifier name) loc : T.Token T.Equals _ : rest) =
    S.Definition loc name <$> (parseExpression rest)
parseDefinition _ =
    Left IncompleteDefinition


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
