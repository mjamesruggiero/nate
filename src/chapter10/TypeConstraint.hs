data OrdStack a = Bottom
                | Item a (OrdStack a)
                  deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b   = isIncreasing rest
    | otherwise = False
isIncreasing _ = True
