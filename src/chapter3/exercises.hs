import Data.Ord
import Data.List

numberOfElems :: [a] -> Int
numberOfElems [] = 0
numberOfElems (x:xs) = 1 + numberOfElems(xs)

mean :: [Float] -> Float
mean []  = 0
mean xs  = sum xs / fromIntegral (length xs)

palindrome :: [a] -> [a]
palindrome []     = []
palindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortLists :: [[a]] -> [[a]]
sortLists lol = sortBy (\a b -> compare (length a) (length b)) lol
