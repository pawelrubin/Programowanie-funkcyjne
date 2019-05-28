import Data.Ord
import Data.List

-- Excercises
-- 1
countMembers :: [a] -> Int
countMembers (x:xs) = 1 + (countMembers xs)
countMembers _      = 0

-- 3
listMean :: [Int] -> Double
listMean (x:xs) = (fromIntegral (x + (sum xs))) / (fromIntegral ((countMembers xs)+1))
listMean _      = 0

-- 4
makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ (drome xs)
    where
        drome ds
            | length ds > 0 = [(last ds)] ++ (drome (init ds))
            | otherwise     = []

-- 5 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs 
    | (length xs) < 2 = True
    | otherwise = (((head xs) == (last xs)) && (isPalindrome (tail (init xs))))

-- 6
sortByLength :: [[a]] -> [[a]]
sortByLength xss = (sortBy (\as bs -> compare (length as) (length bs)) xss)

-- 7
concatLists :: a -> [[a]] -> [a]
concatLists separator xs
    | (length xs) < 2 = (head xs)
    | otherwise = (head xs) ++ [separator] ++ (concatLists separator (tail xs))

-- 8
data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)


-- 9
data Direction =
    RD
    | LD
    | SD
    deriving (Show)

-- 10
data Point =  Point Double Double

turn :: Point -> Point -> Point -> Direction
turn a b c 
    | a1 > a2   = RD
    | a1 == a2  = SD
    | a1 < a2   = LD
    where 
        a1 = ((a. - b.getY) / (a.getX - b.getX)) :: Double
        a2 = ((b.getY - c.getY) / (b.getX - c.getX)) :: Double

