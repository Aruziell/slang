module Main (main) where

import System.Process

import Tokenizer
import Parser
import CodeGenerator


main :: IO ()
main = do
    let input = "1"
    let noOutput result = (result, "")
    let noOutputIO = fmap noOutput
    let outputResult output = (output, output)
    let outputResultIO = fmap outputResult

    let tokenizeAction = noOutput $ tokenize input
    token <- logAction "Tokenize" $ return tokenizeAction

    let parseAction = noOutput $ parse token
    ast <- logAction "Parse" $ return parseAction

    let generateWatAction = outputResult $ generateWat ast
    wat <- logAction "Generate WAT" $ return generateWatAction

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


logAction :: String -> IO (a, String) -> IO a
logAction name action = do
    let info message = "σ: " ++ message

    putStr $ info name ++ ".."
    (result, output) <- action
    putStrLn " ✔"

    case output of
        "" -> return ()
        _ -> do
            putStrLn . unlines $ map ("> " ++) (lines output)

    return result
