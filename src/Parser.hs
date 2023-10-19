{-# LANGUAGE RankNTypes #-}

module Parser
    ( ProgramParser
    , ParseError(..)
    , parse
    , parseExpression
    , parseFunction
    , parseFunctionList
    ) where

import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack, prettyCallStack, callStack)

import qualified Location as L
import qualified Syntax as S
import qualified Token as T


type Parser a = HasCallStack => [T.Token] -> Either ParseError a
type PartialParser a = Parser (a, [T.Token])
type ProgramParser = Parser S.Program


data ParseError
    = IncompleteFunction L.Location
    | IncompleteExpression
    | MissingMain
    | Expectation String Expectation (Maybe T.Token)
    deriving (Eq, Show)


data Expectation
    = Function
    | Expression
    | Operator
    | Term
    | Token T.Value
    deriving (Eq, Show)


toLocation :: [T.Token] -> L.Location
toLocation (T.Token _ loc : _) = loc
toLocation _ = L.Eof


expectError :: HasCallStack => Expectation -> [T.Token] -> ParseError
expectError expect tokens =
    Expectation (prettyCallStack callStack) expect (listToMaybe tokens)


trim :: [T.Token] -> [T.Token]
trim (T.Token T.Newline _ : rest) = trim rest
trim tokens = tokens


parse :: Parser S.Program
parse tokens = do
    (main, funTokens) <- parseMain (trim tokens)
    functionList <- parseFunctionList funTokens
    return $ S.Program main functionList


parseMain :: PartialParser S.Main
parseMain (T.Token (T.Identifier "main") loc : T.Token T.Equals _ : exprAndRest) = do
    (expr, rest) <- indented parseExpression (trim exprAndRest)
    return (S.Main loc expr, rest)
parseMain _ =
    Left MissingMain


parseFunctionList :: Parser [S.Function]
parseFunctionList [] = Right []
parseFunctionList (T.Token T.Newline _ : rest) = parseFunctionList rest
parseFunctionList tokens = do
    (fun, rest) <- parseFunction (trim tokens)
    (fun:) <$> parseFunctionList rest


parseFunction :: PartialParser S.Function
parseFunction (T.Token (T.Identifier name) loc : argsAndRest) = do
    (args, eqAndRest) <- parseArgumentList argsAndRest
    (_, indExpr) <- require T.Equals eqAndRest

    (expr, rest) <- indented parseExpression (trim indExpr)

    return (S.Function loc name args expr, rest)

parseFunction tokens =
    Left $ Expectation "while parsing function" Function (listToMaybe tokens)


indented :: (PartialParser a) -> PartialParser a
indented parser input = do
    let skipped = skipBegin (trim input)

    (parsed, endAndRest) <- parser skipped

    (_, rest) <- if skipped /= input
        then require T.End (trim endAndRest)
        else return ((), endAndRest)

    return (parsed, rest)


skipBegin :: [T.Token] -> [T.Token]
skipBegin (T.Token T.Begin _ : rest) = rest
skipBegin tokens = tokens


parseArgumentList :: PartialParser [S.Argument]
parseArgumentList tokens@(T.Token T.Equals _ : _) =
    return ([], tokens)
parseArgumentList tokens = do
    (headArg, tailTokens) <- parseArgument tokens
    (tailArgs, restTokens) <- parseArgumentTail tailTokens
    return (headArg : tailArgs, restTokens)


parseArgumentTail :: PartialParser [S.Argument]
parseArgumentTail (T.Token T.Separator _ : argAndRest) = do
    (arg, tailAndRest) <- parseArgument argAndRest
    (argTail, rest) <- parseArgumentTail tailAndRest
    return (arg : argTail, rest)
parseArgumentTail tokens =
    return ([], tokens)


parseArgument :: PartialParser S.Argument
parseArgument (T.Token (T.Identifier name) loc : rest) =
    return (S.Argument loc name, rest)
parseArgument tokens =
    Left $ IncompleteFunction (toLocation tokens)


parseExpression :: PartialParser S.Expression
parseExpression (T.Token T.ParenthesisLeft loc : exprAndRest) = do
    (expr, closeAndRest) <- parseExpression exprAndRest
    (_, rest) <- require T.ParenthesisRight closeAndRest
    return (S.Expression (S.Parenthesized expr) loc, rest)
parseExpression (T.Token T.When loc : exprAndRest) = do
    (expr, beginAndRest) <- parseExpression exprAndRest
    (_, casesAndRest) <- require T.Begin (trim beginAndRest)
    (cases, elseAndRest) <- parseWhenCaseList casesAndRest
    (else_, endAndRest) <- parseElse (trim elseAndRest)
    (_, rest) <- require T.End (trim endAndRest)
    return (S.Expression (S.When expr cases else_) loc, rest)
parseExpression tokens =
    parseComparison tokens


parseWhenCaseList :: PartialParser [S.WhenCase]
parseWhenCaseList tokens@(T.Token T.Newline _ : T.Token T.Else _ : _) =
    return ([], tokens)
parseWhenCaseList tokens = do
    (case_, tailTokens) <- parseWhenCase (trim tokens)
    (caseTail, rest) <- parseWhenCaseList tailTokens
    return (case_ : caseTail, rest)


parseWhenCase :: PartialParser (S.Expression, S.Expression)
parseWhenCase tokens = do
    (case_, thenAndRest) <- parseExpression tokens
    (_, resAndRest) <- require T.Then thenAndRest
    (result, rest) <- parseExpression resAndRest
    return ((case_, result), rest)


parseElse :: PartialParser S.Expression
parseElse (T.Token T.Else _ : exprAndRest) = do
    (expr, rest) <- parseExpression exprAndRest
    return (expr, rest)
parseElse tokens =
    Left $ expectError (Token T.Else) tokens


require :: T.Value -> PartialParser ()
require v [] = Left $ expectError (Token v) []
require v tokens@(T.Token value _ : rest)
    | v == value = return ((), rest)
    | otherwise = Left $ expectError (Token v) tokens


parseComparison :: PartialParser S.Expression
parseComparison tokens = do
    (left, opAndRest) <- parseAdditiveExpression tokens
    parseComparisonRest left opAndRest


parseComparisonRest :: S.Expression -> PartialParser S.Expression
parseComparisonRest left tokens = do
    (op, rest) <- maybeComparisonOperator tokens
    case op of
        Just op -> do
            (right, rest) <- parseAdditiveExpression rest
            let expr = S.Expression (S.Logical op left right) (S.comparisonLocation op)
            parseComparisonRest expr rest
        Nothing ->
            return (left, tokens)


maybeComparisonOperator :: PartialParser (Maybe S.ComparisonOperator)
maybeComparisonOperator (T.Token T.GreaterThan loc : rest) =
    return (Just $ S.GreaterThan loc, rest)
maybeComparisonOperator tokens =
    return (Nothing, tokens)


parseAdditiveExpression :: PartialParser S.Expression
parseAdditiveExpression tokens@(T.Token (T.Integer _) _ : _) = do
    (left, addAndRest) <- parseTerm tokens
    parseAdditiveRest left addAndRest
parseAdditiveExpression tokens@(T.Token (T.Identifier _) _ : _) = do
    (left, addAndRest) <- parseCall tokens
    parseAdditiveRest left addAndRest
parseAdditiveExpression tokens =
    Left $ expectError Expression tokens


parseAdditiveRest :: S.Expression -> PartialParser S.Expression
parseAdditiveRest left tokens = do
    (bop, rightAndRest) <- maybeBinaryOperator tokens
    case bop of
        Just op -> do
            (right, rest) <- parseTerm rightAndRest
            let expr = S.Expression (S.BinaryOperator op left right) (S.operatorLocation op)
            parseAdditiveRest expr rest
        Nothing ->
            return (left, tokens)


maybeBinaryOperator :: PartialParser (Maybe S.Operator)
maybeBinaryOperator (T.Token T.Plus loc : rest) =
    return (Just $ S.Add loc, rest)
maybeBinaryOperator (T.Token T.Minus loc : rest) =
    return (Just $ S.Sub loc, rest)
maybeBinaryOperator tokens =
    return (Nothing, tokens)


parseTerm :: PartialParser S.Expression
parseTerm tokens@(T.Token (T.Integer _) _ : _) =
    parseLiteral tokens
parseTerm tokens@(T.Token (T.Identifier _) _ : _) =
    parseCall tokens
parseTerm tokens =
    Left $ expectError Term tokens


parseLiteral :: PartialParser S.Expression
parseLiteral (T.Token (T.Integer value) loc : rest) =
    return (S.Expression (S.Literal (S.Integer value)) loc, rest)
parseLiteral tokens =
    Left $ expectError Expression tokens


parseCall :: PartialParser S.Expression
parseCall (T.Token (T.Identifier name) loc : argsAndRest) = do
    (args, rest) <- parseCallArgumentList argsAndRest
    return (S.Expression (S.FunctionCall name args) loc, rest)
parseCall _ =
    Left IncompleteExpression


parseCallArgumentList :: PartialParser [S.Expression]
parseCallArgumentList tokens@(T.Token T.ParenthesisLeft _ : _) = do
    (expr, argRest) <- parseExpression tokens
    (exprTail, rest) <- parseCallArgumentList argRest
    return (expr : exprTail, rest)
parseCallArgumentList (T.Token (T.Identifier name) loc : argAndRest) = do
    (expr, _) <- parseExpression [T.Token (T.Identifier name) loc]
    (exprTail, rest) <- parseCallArgumentList argAndRest
    return (expr : exprTail, rest)
parseCallArgumentList (T.Token (T.Integer value) loc : argAndRest) = do
    (expr, _) <- parseExpression [T.Token (T.Integer value) loc]
    (exprTail, rest) <- parseCallArgumentList argAndRest
    return (expr : exprTail, rest)
parseCallArgumentList tokens =
    return ([], tokens)
