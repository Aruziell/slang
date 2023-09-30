module CodeGenerator (program, function, expression) where

import qualified Syntax as S


program :: S.Program -> String
program (S.Program (S.Main _ main) defs) =
    "(module\n" ++
    "    (func (export \"_start\") (result i32)\n" ++
    join (_indent ++ _indent) "\n" (expression [] main) ++
    "    )\n" ++
    join _indent "\n" (defs >>= function) ++
    ")\n"


function :: S.Function -> [String]
function (S.Function _ name args expr) =
    [ "(func $" ++ name ++ " " ++ functionArgumentList args ++ "(result i32)"
    ] ++ map (_indent ++) (expression (_argsToScope args) expr) ++
    [ ")"
    ]


_argsToScope :: S.ArgumentList -> [String]
_argsToScope [] = []
_argsToScope (S.Argument _ name : rest) = name : _argsToScope rest


functionArgumentList :: S.ArgumentList -> String
functionArgumentList = concat . map (++ " ") . map functionArgument


functionArgument :: S.Argument -> String
functionArgument (S.Argument _ name) = "(param $" ++ name ++ " i32)"


expression :: [String] -> S.Expression -> [String]
expression locals (S.Expression value _) = expressionValue locals value


expressionValue :: [String] -> S.ExpressionValue -> [String]
expressionValue locals (S.Literal (S.Identifier name)) =
    (if name `elem` locals then _localGet else _call) name
expressionValue _ (S.Literal (S.Integer value)) =
    _i32Const value
expressionValue locals (S.PlusOperator (S.Expression lhs _) (S.Expression rhs _)) =
    -- While it is possible to generate both expressions and then add operator,
    -- we want to generate readable code. This way we'll have operator added
    -- as soon as stack contains enough operands.
    expressionValue locals lhs ++ plusRest locals rhs


plusRest :: [String] -> S.ExpressionValue -> [String]
plusRest locals (S.PlusOperator (S.Expression a _) (S.Expression b _)) =
    expressionValue locals a ++ _i32Add ++ plusRest locals b
plusRest locals value =
    expressionValue locals value ++ _i32Add


join :: String -> String -> [String] -> String
join prefix postfix content =
    content >>= (prefix ++) . (++ postfix)


_indent :: String
_indent = "    "


_call :: String -> [String]
_call = (:[]) . ("call $" ++)


_i32Add :: [String]
_i32Add = ["i32.add"]


_i32Const :: Int -> [String]
_i32Const = (:[]) . ("i32.const " ++) . show


_localGet :: String -> [String]
_localGet = (:[]) . ("local.get $" ++)
