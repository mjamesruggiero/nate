fromMaybe defval wrapped = 
    case wrapped of
        Nothing -> defval
        Just value -> value

data Fruit = Apple | Orange deriving (Show)

whichFruit :: String -> Maybe Fruit

whichFruit f = case f of
                "apple" -> Just Apple
                "orange" -> Just Orange
                _ -> Nothing
