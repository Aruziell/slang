module Parser (ParseError(..), parse, parseDefinition, parseExpression) where

import qualified Syntax as S
import qualified Token as T


type Parser a = [T.Token] -> Either ParseError a
type PartialParser a = Parser (a, [T.Token])


data ParseError
    = IncompleteDefinition
    | IncompleteExpression
    | MissingMain
    deriving (Eq, Show)


parse :: Parser S.Program
parse tokens = do
    (main, defTokens) <- parseMain tokens
    definitionList <- parseDefinitionList defTokens
    return $ S.Program main definitionList


parseMain :: PartialParser S.Main
parseMain (T.Token (T.Identifier "main") loc : T.Token T.Equals _ : exprAndRest) = do
    (expr, rest) <- parseExpression exprAndRest
    return (S.Main loc expr, rest)
parseMain _ =
    Left MissingMain


parseDefinitionList :: Parser [S.Definition]
parseDefinitionList [] = Right []
parseDefinitionList tokens = do
    (def, rest) <- parseDefinition tokens
    (def:) <$> parseDefinitionList rest


parseDefinition :: PartialParser S.Definition
parseDefinition (T.Token (T.Identifier name) loc : T.Token T.Equals _ : exprAndRest) = do
    (expr, rest) <- parseExpression exprAndRest
    return (S.Definition loc name expr, rest)
parseDefinition _ =
    Left IncompleteDefinition


parseExpression :: PartialParser S.Expression
parseExpression tokens@(T.Token (T.Integer _) _ : T.Token T.Plus _ : T.Token (T.Integer _) _ : _) =
    parseAdd tokens
parseExpression (T.Token (T.Integer _) _ : T.Token T.Plus _ : _) =
    -- Missing right operand
    Left IncompleteExpression
parseExpression (T.Token (T.Integer value) loc : rest) =
    return (S.Expression (S.IntegerLiteral value) loc, rest)
parseExpression _ =
    Left IncompleteExpression


parseAdd :: PartialParser S.Expression
parseAdd (T.Token (T.Integer lhs) lLoc : T.Token T.Plus opLoc : rhs@(T.Token (T.Integer _) _ : _)) = do
    let left = S.Expression (S.IntegerLiteral lhs) lLoc
    (right, rest) <- parseExpression rhs
    return (S.Expression (S.PlusOperator left right) opLoc, rest)
parseAdd _ =
    Left IncompleteExpression
