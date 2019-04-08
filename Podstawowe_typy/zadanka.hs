-- Zadanie 20
-- https://wiki.haskell.org/Converting_numbers

-- Zadanie 22
fib :: Int -> Integer
fib n = fibs!!n where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Zadanie 23
middle :: Int -> Int -> Int -> Int
middle x y z 
    | x > y && x < z = x
    | y > x && y < z = y
    | z > y && z < x = z
    | otherwise = 14

-- Zadanie 24
qs :: [Int] -> [Int]
qs [] = []
qs [x] = [x]
qs (x:xs) = (qs [t| t<- xs, t<= x]) ++ [x] ++ (qs [t| t<- xs, t>x])

-- qs' :: [Int] -> [Int]
-- qs' [x] = [x]
-- qs xs = (qs [t| t<- xs, t<= k] ++ [k] ++ (qs [t| t<- xs, t>k])
    -- where k = head
-- Zadanie 25
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' xs = (inits' (init xs)) ++ [xs]

inits'' xs = [take n xs | n <- [0..length xs]]

-- Zadanie 26
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

partitions :: Eq a => [a] -> [([a], [a])]
partitions [] = []
partitions xs = 
    [(a, b) | a <- sub, b <- sub, (a ++ b) == xs]
    where 
        sub = subsets xs

-- Zadanie 27
nub :: Eq a => [a] -> [a]
nub xs = helper [] xs
    where 
        helper acc (x:xs) = 
            if not (x `elem` acc) then
                helper (acc ++ [x]) xs
            else 
                helper acc xs
        helper acc [] = acc

-- Zadanie 28
remove_from_list :: Eq a => a -> [a] -> [a]
remove_from_list d xs = [x | x<-xs, x /= d]
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:ys | y<-xs, ys<-permutations (remove_from_list y xs)]

-- Zadanie 29
trailing_zeros :: Integer -> Integer
trailing_zeros n 
    | divided == 0 = 0
    | otherwise = divided + trailing_zeros divided
    where divided = n `div` 5


rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

rev' :: [a] -> [a]
rev' xs = foldl (++) xs []

rev'' :: [a] -> [a]
rev'' xs = helper xs []
    where 
        helper [] ac = ac
        helper (x:xs) ac = helper xs ([x] ++ ac)