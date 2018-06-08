import Data.List

isPalindrome :: Int -> Bool
isPalindrome num = show num == (reverse . show) num

findAll :: [Int]
findAll = [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

biggest :: [Int] -> Int
biggest list = (head . reverse . sort) list