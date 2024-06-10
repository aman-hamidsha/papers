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




-- 2



-- a

ofSize :: Int -> [a] -> [[a]]
ofSize n ls = map (\(x,y,z) -> [x,y,z]) [(a,b,c) | a <- ls, b <- ls, c <- ls]









-- 3
--a 
-- i 
data Layout a = Element a
    | Vertical (Layout a) (Layout a)
    | Horizontal (Layout a) (Layout a)

instance Functor Layout where 
    fmap :: (a -> b) -> Layout a -> Layout b
    fmap f (Element x) = Element (f x)
    fmap f (Vertical l1 l2) = Vertical (fmap f l1) (fmap f l2)
    fmap f (Horizontal l1 l2) = Horizontal (fmap f l1) (fmap f l2)


instance Foldable Layout where
    foldr :: (a -> b -> b) -> b -> Layout a -> b
    foldr f acc (Element x) = f x acc 
    foldr f acc (Vertical l1 l2) =   (foldr f acc l1) <> (foldr f acc l2)
