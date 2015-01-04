a = ("Porpoise", "Grey")
b = ("Table", "Oak")

data Cretacean = Cretacean String String deriving (Show)
data Furniture = Furniture String String deriving (Show)

c = Cretacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"
