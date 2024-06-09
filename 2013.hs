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
anPlusOne :: Float -> Float -> Float
anPlusOne an x = (an+(x/an))/2

nsqrt :: Float -> Float 
nsqrt x = foldr anPlusOne x [1..10]


-- q3

-- a
charPairs = zip ['a'..'z'] [1..26]
ord :: Char -> Int
ord c = snd $ head $ filter (\(x,y) -> x == c) charPairs

-- b (ASK)


-- primes :: Int -> [Int]
-- primes 2 = [2]
-- primes (x:xs) = filter ()


-- q4 (ASK) 

data List a = Null | Cons a (List a) deriving Show

-- a
head' :: List a -> a
head' (Cons x _) = x
head' Null = error "Empty List"


-- b 
tail' :: List a -> List a
tail' (Cons x xs) = xs
tail' Null = Null


-- c
append' :: List a -> List a -> List a
append' Null ys = ys
append' (Cons x xs) ys = Cons x (append' xs ys)

-- append' :: List a -> List a -> List a
-- append' xs ys = foldr Cons ys xs

-- d
drop' :: Int -> List a -> List a 
drop' 0 l = l
drop' 1 l = l 
drop' n Null = Null
drop' n (Cons _ xs) = drop' (n - 1) xs


-- 5


-- a (ASK)
for :: Int -> Int -> a -> (a -> a) -> a
for n m s f | n>m = s
            | otherwise = for (n+1) m (f s) f


pow :: Float -> Int -> Float  -- pow 2.0 3 => 8.0
pow a n = for 1 (n-1) a (a*)


sumints :: Int -> Int 
sumints n = error "not implemented"


repeatuntil :: a -> (a -> Bool) -> (a -> a) -> a
repeatuntil s p f | p s = s
                  | otherwise = repeatuntil (f s) p f

log2 :: Float -> Float
log2 n = repeatuntil n (< 2) (/2)