-- oof boutta start this one boii


-- 1 a
{-

i. 

([tail,drop 4, take 2] !! 1) "hope"
(drop 4) "hope"
""

ii. (ASK - Ordering) 

map (+) [1,2,3]
(+) 1 : map (+) [2,3]
(+) 1 : (+) 2 : map (+) [3]
(+) 1 : (+) 2 : (+) 3 : map (+) []
(+) 1 : (+) 2 : (+) 3 : []
(+1) : (+2) : (+3) : []
[(+1),(+2),(+3)]

iii. 

if True && x then (\x -> x) (\x -> x) else y



iv. 
map ($ x) $ drop 1 [even, odd, not . even] 
map ($ x) $ [odd, not . even]


v. 

not . not . (&&) True

foldr (\x r -> (.) not x : r) [] [not, (&&) True]

-}


-- b 

-- i . 

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f  = foldr (\x acc -> f x ++ acc ) []




-- ii 




-- iii 
-- ASK 
concatMapM' :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatMapM' f  = foldr (\x acc -> (++) <$> f x <*> acc) (pure [])
