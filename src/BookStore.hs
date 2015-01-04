data BookInfo = Book Int String [String]
                 deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 837042983740 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerId = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerId ReviewBody
                  deriving (Show)
type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerId
                   deriving (Show)

bookID  (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors


