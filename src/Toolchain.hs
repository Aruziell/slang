{-# LANGUAGE RankNTypes #-}

module Toolchain
    ( Toolchain, RunToolchain, SlangError(..)
    , createToolchain, defaultToolchain
    , withRunner, defaultRunToolchain
    ) where

import Data.Bifunctor (first)
import Control.Monad.Trans.Except

import CodeGenerator (ProgramGenerator, program)
import Parser (ProgramParser, ParseError, parse)
import Runner (Runner, RunSuccess, RunError, wasmtimeRunner)
import Tokenizer (Tokenizer, TokenizeError, tokenize)

import qualified Syntax as S
import qualified Token as T


type Toolchain = String -> Either SlangError String
type RunToolchain = String -> IO (Either SlangError RunSuccess)


data SlangError
    = TokenizeFailure TokenizeError
    | ParseFailure ParseError
    | RunFailure RunError
    deriving (Eq, Show)


createToolchain :: Tokenizer -> ProgramParser -> ProgramGenerator -> Toolchain
createToolchain tokenizer parser generator input =
    liftTokenizerError tokenizer input
        >>= liftParserError parser
        >>= return . generator


defaultToolchain :: Toolchain
defaultToolchain = createToolchain tokenize parse program

defaultRunToolchain :: RunToolchain
defaultRunToolchain = defaultToolchain `withRunner` wasmtimeRunner


liftTokenizerError :: Tokenizer -> String -> Either SlangError [T.Token]
liftTokenizerError = fmap (first TokenizeFailure)


liftParserError :: ProgramParser -> [T.Token] -> Either SlangError S.Program
liftParserError = fmap (first ParseFailure)


liftRunnerError :: Runner -> String -> IO (Either SlangError RunSuccess)
liftRunnerError = fmap (fmap (first RunFailure))


withRunner :: Toolchain -> Runner -> RunToolchain
withRunner toolchain runner input = runExceptT $ do
    output <- ExceptT $ return $ toolchain input
    result <- ExceptT $ (liftRunnerError runner) output
    return result
