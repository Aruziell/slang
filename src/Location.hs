module Location (Location(..)) where


data Location
    = Location
        { line :: Int
        , column :: Int
        }
    | Eof
    deriving (Eq)


instance Show Location where
    show (Location l c) = show l ++ ":" ++ show c
    show Eof = "End of file"
