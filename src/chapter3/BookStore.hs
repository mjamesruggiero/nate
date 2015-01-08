data BookInfo = Book Int String [String]
                 deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 837042983740 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookID  (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerId      (Book id _ _ ) = id
nicerTitle   (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
    customerID :: CustomerID
    , customerName  :: String
    , customerAddress :: Address
} deriving (Show)

customer2 = Customer {
    customerID = 267890
    , customerAddress = ["12345 Disk Drive", "Milpitas, CA 94501", "USA"]
    , customerName = "John Doe"
}
