module Main (main) where

import System.Process

import Location (Location(Location))
import Tokenizer
import Parser
import CodeGenerator
import Data.Bifunctor (first)


input :: String
input = concat $ map (++ "\n")
    [ "main = 1 + 2 + foo + 4 + bar"
    , "foo = 3"
    , "bar = 5"
    ]


main :: IO ()
main = do
    let noOutput result = (result, "")
    let noOutputIO = fmap noOutput
    let outputResult output = (output, output)
    let outputResultIO = fmap outputResult

    let tokenizeAction = first TokenizeFailure $ tokenize input
    tokens <- logActionEither "Tokenize" $ noOutput <$> tokenizeAction

    let parseAction = noOutput $ first ParseFailure $ parse tokens
    ast <- logActionEither "Parse" $ return parseAction

    let generateWatAction = outputResult <$> program <$> ast
    wat <- logActionEither "Generate WAT" $ generateWatAction

    let writeFileAction = noOutputIO $ writeFile "main.wat" wat
    _ <- logAction "Write WAT file" writeFileAction

    let compileWatAction = outputResultIO $ wat2wasm "main.wat" "main.wasm"
    _ <- logAction "Compile WAT to WASM" compileWatAction

    let run = outputResultIO $ wasmtime "main.wasm"
    _ <- logAction "Execute WASM with wasmtime" run

    return ()


wat2wasm :: FilePath -> FilePath -> IO String
wat2wasm inputFile outputFile =
    readProcess "wat2wasm" [inputFile, "-o", outputFile] []


wasmtime :: FilePath -> IO String
wasmtime file =
    readProcess "wasmtime" [file] []


locationToString :: Location -> String
locationToString (Location line column) =
    show line ++ ":" ++ show column


data SlangError
    = TokenizeFailure TokenizeError
    | ParseFailure ParseError


errorMessage :: SlangError -> String
errorMessage (TokenizeFailure err) = tokenizeErrorMessage err
errorMessage (ParseFailure err) = parseErrorMessage err


tokenizeErrorMessage :: TokenizeError -> String
tokenizeErrorMessage (IllegalCharacter c loc) =
    "Illegal character " ++ [c] ++ " at " ++ locationToString loc


parseErrorMessage :: ParseError -> String
parseErrorMessage IncompleteExpression = "Incomplete expression."
parseErrorMessage IncompleteDefinition = "Incomplete definition."
parseErrorMessage MissingMain = "First definition must be main."


logActionEither :: String -> Either SlangError (a, String) -> IO a
logActionEither name action = do
    let info message = "σ " ++ message

    putStr $ info name ++ ".."

    case action of
        Left err -> do
            putStrLn " ✘"
            fail $ errorMessage err
        Right (result, output) -> do
            putStrLn " ✔"
            case output of
                "" ->
                    return result
                _ -> do
                    putStrLn . unlines $ map ("> " ++) (lines output)
                    return result


logAction :: String -> IO (a, String) -> IO a
logAction name action = do
    let info message = "σ " ++ message

    putStr $ info name ++ ".."
    (result, output) <- action
    putStrLn " ✔"

    case output of
        "" -> return ()
        _ -> do
            putStrLn . unlines $ map ("> " ++) (lines output)

    return result
