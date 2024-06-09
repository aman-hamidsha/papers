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


--  6 


-- a
-- foldr :: ()
concat' :: [[a]] -> [a]
concat'  = foldr (++) []

--b
map' :: (a -> b) -> [a] -> [b]
map' f ls = foldr (\x accList -> [(f x)] ++ accList) [] ls

-- map' f = foldr (\x accList -> (f x) : accList) []

-- c
length' :: [a] -> Int
length' = foldl (\acc x -> 1 + acc) 0  

-- d
reverse' :: [a] -> [a]
reverse' ls = foldl (\acc x -> [x] ++ acc ) [] ls

-- result of thing in brackets will be new acc

{-
Iteration 1:
Element: 1
Accumulator (acc): []
Function Application: (\acc x -> [x] ++ acc) [] 1
Result: [1] ++ [] which is [1]
New Accumulator: [1]
Iteration 2:
Element: 2
Accumulator (acc): [1]
Function Application: (\acc x -> [x] ++ acc) [1] 2
Result: [2] ++ [1] which is [2, 1]
New Accumulator: [2, 1]
Iteration 3:
Element: 3
Accumulator (acc): [2, 1]
Function Application: (\acc x -> [x] ++ acc) [2, 1] 3
Result: [3] ++ [2, 1] which is [3, 2, 1]
New Accumulator: [3, 2, 1]
Iteration 4:
Element: 4
Accumulator (acc): [3, 2, 1]
Function Application: (\acc x -> [x] ++ acc) [3, 2, 1] 4
Result: [4] ++ [3, 2, 1] which is [4, 3, 2, 1]
New Accumulator: [4, 3, 2, 1]
After all iterations are complete, the final value of the accumulator is [4, 3, 2, 1], which is the reversed list.

Summary:
Initial list: [1, 2, 3, 4]
After folding:
[1]
[2, 1]
[3, 2, 1]
[4, 3, 2, 1]
Final reversed list: [4, 3, 2, 1]


-}