--2 

-- a

length' :: [a] -> Int
length' = foldr (\x acc -> acc + 1) 0 

binary' :: Int -> [Char]
binary' n = foldr (\) "" n