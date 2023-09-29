module Parser
    ( ParseError(..)
    , parse
    , parseDefinition
    , parseDefinitionList
    , parseExpression
    ) where

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
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- parseEnd endAndRest
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
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- parseEnd endAndRest
    return (S.Definition loc name expr, rest)
parseDefinition _ =
    Left IncompleteDefinition


parseExpression :: PartialParser S.Expression
parseExpression = parseAdd


parseAdd :: PartialParser S.Expression
parseAdd tokens = do
    (left, addAndRest) <- parseLiteral tokens
    case addAndRest of
        (T.Token T.Plus loc : rightTokens) -> do
            (right, rest) <- parseExpression rightTokens
            return (S.Expression (S.PlusOperator left right) loc, rest)
        _ ->
            return (left, addAndRest)


parseLiteral :: PartialParser S.Expression
parseLiteral (T.Token (T.Identifier name) loc : rest) =
    return (S.Expression (S.Literal (S.Identifier name)) loc, rest)
parseLiteral (T.Token (T.Integer value) loc : rest) =
    return (S.Expression (S.Literal (S.Integer value)) loc, rest)
parseLiteral _ =
    Left IncompleteExpression


parseEnd :: PartialParser ()
parseEnd ((T.Token T.End _) : rest) =
    return ((), rest)
parseEnd _ =
    Left IncompleteDefinition
