-- Zadanie 22
fib :: Int -> Integer
fib n = fibs!!n where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Zadanie 23
middle :: Int -> Int -> Int -> Int
middle x y z 
    | x > y && x < z = x
    | y > x && y < z = y
    | z > y && z < x = z
    | otherwise = undefined

-- Zadanie 24
qs :: [Int] -> [Int]
qs [] = []
qs [x] = [x]
qs (x:xs) = (qs [t| t<- xs, t<= x]) ++ [x] ++ (qs [t| t<- xs, t>x])

-- Zadanie 25
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' xs = (inits' (init xs)) ++ [xs]

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
        helper acc _ = acc

-- Zadanie 28

-- Zadanie 29
trailing_zeros :: Integer -> Integer
trailing_zeros n 
    | divided == 0 = 0
    | otherwise = divided + trailing_zeros divided
    where divided = n `div` 5
