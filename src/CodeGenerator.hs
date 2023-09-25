module CodeGenerator (generateWat) where

import qualified Syntax as S


generateWat :: S.Program -> String
generateWat (S.Program (S.Expression value _)) =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    join "        " "\n" (expressionValue value) ++
    "    )\n" ++
    ")\n"


expressionValue :: S.Value -> [String]
expressionValue (S.IntegerLiteral value) =
    _i32Const value
expressionValue (S.PlusOperator (S.Expression lhs _) (S.Expression rhs _)) =
    -- While it is possible to generate both expressions and then add operator,
    -- we want to generate readable code. This way we'll have operator added
    -- as soon as stack contains enough operands.
    expressionValue lhs ++ plusRest rhs


plusRest :: S.Value -> [String]
plusRest (S.PlusOperator (S.Expression a _) (S.Expression b _)) =
    expressionValue a ++ _i32Add ++ plusRest b
plusRest value =
    expressionValue value ++ _i32Add


join :: String -> String -> [String] -> String
join prefix postfix content =
    content >>= (prefix ++) . (++ postfix)


_i32Add :: [String]
_i32Add = ["i32.add"]


_i32Const :: Int -> [String]
_i32Const = (:[]) . ("i32.const " ++) . show
