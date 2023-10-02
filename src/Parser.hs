module Parser
    ( ParseError(..)
    , parse
    , parseExpression
    , parseFunction
    , parseFunctionList
    ) where

import qualified Syntax as S
import qualified Token as T


type Parser a = [T.Token] -> Either ParseError a
type PartialParser a = Parser (a, [T.Token])


data ParseError
    = IncompleteFunction
    | IncompleteExpression
    | MissingMain
    deriving (Eq, Show)


parse :: Parser S.Program
parse tokens = do
    (main, defTokens) <- parseMain tokens
    functionList <- parseFunctionList defTokens
    return $ S.Program main functionList


parseMain :: PartialParser S.Main
parseMain (T.Token (T.Identifier "main") loc : T.Token T.Equals _ : exprAndRest) = do
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- parseEnd endAndRest
    return (S.Main loc expr, rest)
parseMain _ =
    Left MissingMain


parseFunctionList :: Parser [S.Function]
parseFunctionList [] = Right []
parseFunctionList tokens = do
    (def, rest) <- parseFunction tokens
    (def:) <$> parseFunctionList rest


parseFunction :: PartialParser S.Function
parseFunction (T.Token (T.Identifier name) loc : argsAndRest) = do
    (args, eqAndRest) <- parseArgumentList argsAndRest
    (_, exprAndRest) <- parseEquals eqAndRest
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- parseEnd endAndRest
    return (S.Function loc name args expr, rest)
parseFunction _ =
    Left IncompleteFunction


parseArgumentList :: PartialParser [S.Argument]
parseArgumentList tokens@(T.Token T.Equals _ : _) =
    return ([], tokens)
parseArgumentList tokens = do
    (headArg, tailTokens) <- parseArgument tokens
    (tailArgs, restTokens) <- parseArgumentList tailTokens
    return (headArg : tailArgs, restTokens)


parseArgument :: PartialParser S.Argument
parseArgument (T.Token (T.Identifier name) loc : rest) =
    return (S.Argument loc name, rest)
parseArgument _ =
    Left IncompleteFunction


parseEquals :: PartialParser ()
parseEquals (T.Token T.Equals _ : rest) = return ((), rest)
parseEquals _ = Left IncompleteFunction


parseExpressionList :: PartialParser [S.Expression]
parseExpressionList [] = return ([], [])
parseExpressionList tokens@(T.Token (T.Identifier _) _ : _) = do
    (expr, tailAndRest) <- parseExpression tokens
    (exprTail, rest) <- parseExpressionList tailAndRest
    return (expr : exprTail, rest)
parseExpressionList tokens@(T.Token (T.Integer _) _ : _) = do
    (expr, tailAndRest) <- parseExpression tokens
    (exprTail, rest) <- parseExpressionList tailAndRest
    return (expr : exprTail, rest)
parseExpressionList tokens =
    return ([], tokens)


parseExpression :: PartialParser S.Expression
parseExpression = parseAdd


parseAdd :: PartialParser S.Expression
parseAdd tokens@(T.Token (T.Integer _) _ : _) = do
    (left, addAndRest) <- parseLiteral tokens
    parseAddAndRest left addAndRest
parseAdd tokens@(T.Token (T.Identifier _) _ : _) = do
    (left, addAndRest) <- parseCall tokens
    parseAddAndRest left addAndRest
parseAdd _ =
    Left IncompleteExpression


parseAddAndRest :: S.Expression -> PartialParser S.Expression
parseAddAndRest left (T.Token T.Plus loc : rightAndRest) = do
    (right, rest) <- parseExpression rightAndRest
    return (S.Expression (S.PlusOperator left right) loc, rest)
parseAddAndRest left tokens =
    return (left, tokens)


parseLiteral :: PartialParser S.Expression
parseLiteral (T.Token (T.Integer value) loc : rest) =
    return (S.Expression (S.Literal (S.Integer value)) loc, rest)
parseLiteral _ =
    Left IncompleteExpression


parseCall :: PartialParser S.Expression
parseCall (T.Token (T.Identifier name) loc : argsAndRest) = do
    (args, rest) <- parseExpressionList argsAndRest
    return (S.Expression (S.FunctionCall name args) loc, rest)
parseCall _ =
    Left IncompleteExpression


parseEnd :: PartialParser ()
parseEnd ((T.Token T.End _) : rest) =
    return ((), rest)
parseEnd _ =
    Left IncompleteFunction
