-- 2
prod :: [Int] -> Int
prod = foldr (*) 1

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- 3 
data List a = Nil | Cons a (List a)

hd :: List a -> a
hd Nil = error "im so done with this fucking year"
hd (Cons x xs) = x

tl :: List a -> List a
tl Nil = error "im so done with this fucking year"
tl (Cons x xs) = xs


-- MAKES SENSE BUT RECHECK AND STUFF
showl :: Show a => List a -> [Char]
showl list = "[" ++ showElements list ++ "]"
  where
    showElements Nil = ""
    showElements (Cons x Nil) = show x
    showElements (Cons x xs) = show x ++ "," ++ showElements xs


-- 6
-- foldr DEFINTION 
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ acc [] = acc
foldr'' f acc (x:xs) = f x (foldr'' f acc xs)


pow' :: Int -> Int
pow' n = foldr (*) 1 (replicate n 2)