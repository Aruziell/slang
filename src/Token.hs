module Token (Token(..)) where


data Token
    = IntegerToken Int
    | PlusToken
    deriving (Eq, Show)
