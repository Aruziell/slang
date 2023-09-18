module Token (Token(..)) where


data Token
    = IntegerToken Int
    deriving (Eq, Show)
