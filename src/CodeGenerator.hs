module CodeGenerator (generateWat) where

import qualified Syntax as S


generateWat :: S.Program -> String
generateWat (S.Program expression) =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    "        " ++ generateExpression expression ++ "\n" ++
    "    )\n" ++
    ")\n"


generateExpression :: S.Expression -> String
generateExpression (S.IntegerLiteral value) =
    "i32.const " ++ show value
