-- What is the value of the first triangle number to have over five hundred divisors?
import Data.List (find)

triangles :: [Int]
triangles = [sum [1..x] | x <- [1..]]

divisors num = [x | x <- [1..num], mod num x == 0]

divisorsSmall num = [x | x <- [1..(round . sqrt . fromIntegral) num], mod num x == 0]

filterCond :: Int -> Int -> Bool
filterCond limit = (> limit) . length . divisors

filterCondSmall :: Int -> Int -> Bool
filterCondSmall limit = (> (div limit 2)) . length . divisorsSmall

-- too slow, no need to check all divisors
solve limit = find (filterCond limit) triangles

-- predicate that number of all divisors <- sqrt(num) is exactly half of all the divisors
-- efficient enough to produce result for limit = 500
solveE limit = find (filterCondSmall limit) triangles