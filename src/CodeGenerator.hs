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
    _i32Const value
generateExpression (S.PlusOperator lhs rhs) =
    -- While it is possible to generate both expressions and then add operator,
    -- we want to generate readable code. This way we'll have operator added
    -- as soon as stack contains enough operands.
    generateExpression lhs ++ generatePlusRest rhs


generatePlusRest :: S.Expression -> [String]
generatePlusRest (S.PlusOperator a b) =
    generateExpression a ++ _i32Add ++ generatePlusRest b
generatePlusRest expression =
    generateExpression expression ++ _i32Add


join :: String -> String -> [String] -> String
join prefix postfix content =
    content >>= (prefix ++) . (++ postfix)


_i32Add :: [String]
_i32Add = ["i32.add"]


_i32Const :: Int -> [String]
_i32Const = (:[]) . ("i32.const " ++) . show
