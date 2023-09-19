module Main (main) where

import System.Process

import Tokenizer
import Parser
import CodeGenerator


main :: IO ()
main = do
    let input = "1 + 2 + 3 + 4 + 5"
    let noOutput result = (result, "")
    let noOutputIO = fmap noOutput
    let outputResult output = (output, output)
    let outputResultIO = fmap outputResult

    let tokenizeAction = noOutput $ tokenize input
    tokens <- logAction "Tokenize" $ return tokenizeAction

    let parseAction = noOutput $ parse tokens
    ast <- logAction "Parse" $ return parseAction

    let generateWatAction = outputResult <$> generateWat <$> ast
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


errorMessage :: ParseError -> String
errorMessage IncompleteExpression = "Incomplete expression."


logActionEither :: String -> Either ParseError (a, String) -> IO a
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
