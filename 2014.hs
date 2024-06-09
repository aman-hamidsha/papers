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
fac n = n * fac (n-1)

e :: Float -> Int -> Float
e flt n = sum $ map (\x -> flt^x / fac (fromIntegral x)) [0..(fromIntegral n)]


-- 3
-- a
-- ASK 
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs



charPairs = zip ['a'..'z'] [1..26]
ord :: Char -> Int
ord c = snd $ head $ filter (\(x,y) -> x == c) charPairs

chr :: Int -> Char
chr i = fst $ head $ filter (\(x,y) -> y == i) charPairs

caesar :: Int -> String -> String
caesar i str = map chr $ map (\x -> (x+i) `mod` 26) $ map ord str
-- caesar i = map ((chr . (\x -> (x+i) `mod` 26)) . ord)

-- 4 

-- INCOMPLETE (ASK )
data Btree a = 
      Null 
    | Leaf a 
    | B2 (Btree a) a (Btree a) deriving Show


allnodes :: Btree a -> [a]
allnodes Null = []
allnodes (Leaf a) = [a]
allnodes (B2 l a r) = allnodes l ++ allnodes r ++ [a]

equals' :: Eq a => Btree a -> Btree a -> Bool
equals' (B2 l1 a1 r1) (B2 l2 a2 r2) = a1 == a2



-- 5 c

fac' 0 = 1
fac' n = foldl (\acc x -> acc * x ) 1 [1..n] 

-- 6

fac'' 0 = 1
fac'' n = foldr (\x acc -> x * acc ) 1 [1..n] 

-- GOT IT BUT CLARIFY
app' :: [a] -> [a] -> [a]
app' a b = foldr (\x acc -> [x] ++  acc) b a

{-
Here's how the function works:

foldr processes each element of the list a from right to left, applying the function (\x acc -> [x] ++ acc).
The initial accumulator is the list b.
Given the inputs:

a is "abc"
b is "de"
Step-by-Step Evaluation:
Initial State:

a is "abc"
b is "de"
The initial accumulator (acc) is b, which is "de".
Applying foldr from right to left on "abc":

Iteration 1:
Element: 'c'
Accumulator (acc): "de"
Function Application: (\x acc -> [x] ++ acc) 'c' "de"
Result: 'c' : "de" which is "cde"
New Accumulator: "cde"
Iteration 2:
Element: 'b'
Accumulator (acc): "cde"
Function Application: (\x acc -> [x] ++ acc) 'b' "cde"
Result: 'b' : "cde" which is "bcde"
New Accumulator: "bcde"
Iteration 3:
Element: 'a'
Accumulator (acc): "bcde"
Function Application: (\x acc -> [x] ++ acc) 'a' "bcde"
Result: 'a' : "bcde" which is "abcde"
New Accumulator: "abcde"
After all iterations are complete, the final value of the accumulator is "abcde", which is the concatenation of "abc" and "de".

Summary:
Initial lists: "abc" and "de"
After folding:
'c' + "de" = "cde"
'b' + "cde" = "bcde"
'a' + "bcde" = "abcde"
Final concatenated list: "abcde"
Thus, app' "abc" "de" evaluates to "abcde" by folding the elements of the first list ("abc") from right to left and concatenating them to the second list ("de").


-}