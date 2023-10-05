{-# LANGUAGE RankNTypes #-}

module Parser
    ( ParseError(..)
    , parse
    , parseExpression
    , parseFunction
    , parseFunctionList
    ) where

import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack, prettyCallStack, callStack)

import qualified Syntax as S
import qualified Token as T


type Parser a = HasCallStack => [T.Token] -> Either ParseError a
type PartialParser a = Parser (a, [T.Token])


data ParseError
    = IncompleteFunction
    | IncompleteExpression
    | MissingMain
    | Expectation String Expectation (Maybe T.Token)
    deriving (Eq, Show)


data Expectation
    = Function
    | Expression
    | Token T.Value
    deriving (Eq, Show)


expectError :: HasCallStack => Expectation -> [T.Token] -> ParseError
expectError expect tokens =
    Expectation (prettyCallStack callStack) expect (listToMaybe tokens)


parse :: Parser S.Program
parse tokens = do
    (main, funTokens) <- parseMain tokens
    functionList <- parseFunctionList funTokens
    return $ S.Program main functionList


parseMain :: PartialParser S.Main
parseMain (T.Token (T.Identifier "main") loc : T.Token T.Equals _ : exprAndRest) = do
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- requireEnd endAndRest
    return (S.Main loc expr, rest)
parseMain _ =
    Left MissingMain


parseFunctionList :: Parser [S.Function]
parseFunctionList [] = Right []
parseFunctionList tokens = do
    (fun, rest) <- parseFunction tokens
    (fun:) <$> parseFunctionList rest


parseFunction :: PartialParser S.Function
parseFunction (T.Token (T.Identifier name) loc : argsAndRest) = do
    (args, eqAndRest) <- parseArgumentList argsAndRest
    (_, exprAndRest) <- parseEquals eqAndRest
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, rest) <- requireEnd endAndRest
    return (S.Function loc name args expr, rest)
parseFunction _ =
    Left IncompleteFunction


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
parseArgument _ =
    Left IncompleteFunction


parseEquals :: PartialParser ()
parseEquals (T.Token T.Equals _ : rest) = return ((), rest)
parseEquals tokens = Left $ expectError (Token T.Equals) tokens


parseExpression :: PartialParser S.Expression
parseExpression (T.Token T.ParenthesisLeft loc : exprAndRest) = do
    (expr, closeAndRest) <- parseExpression exprAndRest
    (_, rest) <- requireParClose closeAndRest
    return (S.Expression (S.Parenthesized expr) loc, rest)
parseExpression (T.Token T.When loc : exprAndRest) = do
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, casesAndRest) <- requireEnd endAndRest
    (cases, elseAndRest) <- parseWhenCaseList casesAndRest
    (else_, rest) <- parseElse elseAndRest
    return (S.Expression (S.When expr cases else_) loc, rest)
parseExpression tokens =
    parseAdd tokens


parseWhenCaseList :: PartialParser [S.WhenCase]
parseWhenCaseList tokens@(T.Token T.Else _ : _) =
    return ([], tokens)
parseWhenCaseList tokens = do
    (case_, thenAndRest) <- parseExpression tokens
    (_, exprAndRest) <- requireThen thenAndRest
    (expr, endAndRest) <- parseExpression exprAndRest
    (_, caseTailAndRest) <- requireEnd endAndRest
    (caseTail, rest)  <- parseWhenCaseList caseTailAndRest
    return ((case_, expr) : caseTail, rest)


parseElse :: PartialParser S.Expression
parseElse (T.Token T.Else _ : exprAndRest) = do
    (expr, rest) <- parseExpression exprAndRest
    return (expr, rest)
parseElse tokens =
    Left $ expectError (Token T.Else) tokens


requireEnd :: PartialParser ()
requireEnd (T.Token T.End _ : rest) =
    return ((), rest)
requireEnd tokens =
    Left $ expectError (Token T.End) tokens


requireParClose :: PartialParser ()
requireParClose (T.Token T.ParenthesisRight _ : rest) =
    return ((), rest)
requireParClose tokens =
    Left $ expectError (Token T.ParenthesisRight) tokens


requireThen :: PartialParser ()
requireThen (T.Token T.Then _ : rest) =
    return ((), rest)
requireThen tokens =
    Left $ expectError (Token T.Then) tokens


parseAdd :: PartialParser S.Expression
parseAdd tokens@(T.Token (T.Integer _) _ : _) = do
    (left, addAndRest) <- parseLiteral tokens
    parseAddAndRest left addAndRest
parseAdd tokens@(T.Token (T.Identifier _) _ : _) = do
    (left, addAndRest) <- parseCall tokens
    parseAddAndRest left addAndRest
parseAdd tokens =
    Left $ expectError Expression tokens


parseAddAndRest :: S.Expression -> PartialParser S.Expression
parseAddAndRest left (T.Token T.Plus loc : rightAndRest) = do
    (right, rest) <- parseExpression rightAndRest
    return (S.Expression (S.PlusOperator left right) loc, rest)
parseAddAndRest left tokens =
    return (left, tokens)


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
