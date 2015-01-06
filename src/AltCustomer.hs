data Customer = Customer Int String [String]
                deriving (Show)

customerId :: Customer -> Int
customerId (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address
