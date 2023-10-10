module Main (main) where

import Data.Bifunctor (first)
import System.Process

import Tokenizer
import Parser
import CodeGenerator
import Runner
import Toolchain


input :: String
input = unlines
    [ "main ="
    , "    fib 10"
    , "fib n ="
    , "    fibn 0 n 0 1"
    , "fibn c, n, a, b ="
    , "    when c"
    , "        n then a"
    , "        else fibn (c+1) n b (a+b)"
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


errorMessage :: SlangError -> String
errorMessage (TokenizeFailure err) = tokenizeErrorMessage err
errorMessage (ParseFailure err) = parseErrorMessage err
errorMessage (RunFailure err) = runErrorMessage err


tokenizeErrorMessage :: TokenizeError -> String
tokenizeErrorMessage (IllegalCharacter c loc) =
    "Illegal character " ++ [c] ++ " at " ++ show loc


parseErrorMessage :: ParseError -> String
parseErrorMessage IncompleteExpression = "Incomplete expression."
parseErrorMessage (IncompleteFunction loc) =
    "Incomplete function @ " ++ show loc ++ "."
parseErrorMessage MissingMain = "First function must be main."
parseErrorMessage (Expectation desc expectation token) =
    "Unexpected token " ++ show token ++ ".\n"
        ++ "Expected " ++ show expectation ++ ".\n"
        ++ desc


runErrorMessage :: RunError -> String
runErrorMessage (RunError code stdout stderr) =
    "Run failure. Process exited with code " ++ show code ++ ".\n"
        ++ "Output:\n" ++ stdout ++ "\n"
        ++ "Error:\n" ++ stderr ++ "\n"


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
