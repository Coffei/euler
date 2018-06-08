-- Find the sum of all the primes below two million.

solve :: Int -> Int
solve limit = sum (takeWhile (<limit) list_primes)

list_primes :: [Int]
list_primes = [x | x <- [2..], isPrime x]

isPrime :: Int -> Bool
isPrime x = (divisors x) == []

divisors num = [x | x <- [2..(round . sqrt . fromIntegral) num], mod num x == 0]