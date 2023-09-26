module CodeGenerator (program, expression) where

import qualified Syntax as S


program :: S.Program -> String
program (S.Program def) =
    "(module\n" ++
    "    (func (export \"_start\")\n" ++
--    join "        " "\n" (expressionValue value) ++
    "    )\n" ++
    join _indent "\n" (definition def) ++
    ")\n"


definition :: S.Definition -> [String]
definition (S.Definition _ name expr) =
    [ "(func $" ++ name ++ " (result i32)"
    ] ++ map (_indent ++) (expression expr) ++
    [ ")"
    ]


expression :: S.Expression -> [String]
expression (S.Expression value _) = expressionValue value


expressionValue :: S.ExpressionValue -> [String]
expressionValue (S.IntegerLiteral value) =
    _i32Const value
expressionValue (S.PlusOperator (S.Expression lhs _) (S.Expression rhs _)) =
    -- While it is possible to generate both expressions and then add operator,
    -- we want to generate readable code. This way we'll have operator added
    -- as soon as stack contains enough operands.
    expressionValue lhs ++ plusRest rhs


plusRest :: S.ExpressionValue -> [String]
plusRest (S.PlusOperator (S.Expression a _) (S.Expression b _)) =
    expressionValue a ++ _i32Add ++ plusRest b
plusRest value =
    expressionValue value ++ _i32Add


join :: String -> String -> [String] -> String
join prefix postfix content =
    content >>= (prefix ++) . (++ postfix)


_indent :: String
_indent = "    "


_i32Add :: [String]
_i32Add = ["i32.add"]


_i32Const :: Int -> [String]
_i32Const = (:[]) . ("i32.const " ++) . show
