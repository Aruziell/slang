module Token (Location(..), Token(..), Value(..)) where


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = Integer Int
    | Plus
    deriving (Eq, Show)


data Location
    = Location
        { line :: Int
        , column :: Int
        }
    deriving (Eq, Show)
