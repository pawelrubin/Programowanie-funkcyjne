-- Zadanie 48
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan p xs = ([n | n <- xs, p n], [m | m <- xs, not (p m)])

-- otherSpan :: (a -> Bool) -> [a] -> ([a], [a])
-- otherSpan _ [] = ([], [])
-- otherSpan p xs =
--     let helper _ [] _ = ([]. [])
--         helper r (y:ys) acc =
--             if (r y) then helper r ys (acc ++ [y])
--             else (acc, y:ys)
--         in helper p xs []


-- Zadanie 49
ecd :: (Eq a) => [a] -> [a]
ecd [] = []
ecd [x] = [x]
ecd (x:xs)
    | x == (head xs) = ecd xs
    | otherwise = [x] ++ (ecd xs)

ecd2 :: (Eq a) => [a] -> [a]
ecd2 [] = []
ecd2 [x] = [x]
ecd2 (x1:x2:xs)
    | x1 == x2 = ecd x2:xs
    | otherwise = x1 : (ecd xs)
-- Zadanie 50
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack xs =  [sublist] ++ pack (drop (length sublist) (tail xs))
    where sublist = [x | x <- xs, x == head xs]

-- Zadanie 51
-- rleEncode :: [Char] -> [(Int, Char)]
-- rleEncode [] = []
-- rleEncode (x:xs)
--     | x == (head xs) = 

