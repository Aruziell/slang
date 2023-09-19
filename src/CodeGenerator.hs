module CodeGenerator (generateWat) where

import qualified Syntax as S


generateWat :: S.Program -> String
generateWat (S.Program expression) =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    join "        " "\n" (generateExpression expression) ++
    "    )\n" ++
    ")\n"


generateExpression :: S.Expression -> [String]
generateExpression (S.IntegerLiteral value) =
    ["i32.const " ++ show value]
generateExpression (S.PlusOperator lhs rhs) =
    ([lhs, rhs] >>= generateExpression) ++ ["i32.add"]


join :: String -> String -> [String] -> String
join prefix postfix content =
    content >>= (prefix ++) . (++ postfix)
