divisors num = [x | x <- [2..(round . sqrt . fromIntegral) num], mod num x == 0]

containsDivisor :: [Int] -> Int -> Bool
containsDivisor list item = any (\y -> mod item y == 0 && y /= item) list

uniqueDivisors :: [Int] -> [Int]
uniqueDivisors list = filter (\y -> not (containsDivisor list y)) list