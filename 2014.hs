--2 

-- a

length' :: [a] -> Int
length' = foldr (\x acc -> acc + 1) 0 

-- b


-- ASK 
-- binary' :: Int -> [Char]
-- binary' 1 = "1"
-- binary n = 


-- c

fac 0 = 1
fac n = n * fac(n-1)

e :: Float -> Int -> Float
e flt n = sum $ map (\x -> flt^x / fac (fromIntegral x)) [0..(fromIntegral n)]


-- 3
-- a

isort :: Ord a => [a] -> [a]
