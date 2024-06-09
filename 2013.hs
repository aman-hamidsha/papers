-- q2

-- a
sumsq :: Int -> Int
sumsq n = foldr (\x acc -> x^2 + acc) 0 [1..n]


-- b
isPrime :: Int -> Bool 
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = not $ foldr (\x acc -> n `mod` x == 0 || acc) False [2..n-1]



-- c
