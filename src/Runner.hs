module Runner where

import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


type Runner = (String -> IO RunResult)

type RunResult = Either RunError RunSuccess

data RunError = RunError ExitCode String String deriving (Eq, Show)
type RunSuccess = (String, String)


wasmtimeRunner :: Runner
wasmtimeRunner input = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "wasmtime" ["-"] input
    return $ case exitCode of
        ExitFailure _ -> Left (RunError exitCode stdout stderr)
        ExitSuccess -> return (stdout, stderr)
