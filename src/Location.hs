module Location (Location(..)) where


data Location
    = Location
        { line :: Int
        , column :: Int
        }
    deriving (Eq, Show)
