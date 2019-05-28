
-- Zadanie 52
countEven :: [Int] -> Int
countEven xs = foldr ((+).fromEnum.even) 0 xs

-- Zadanie 54
-- foldl' >>> foldr > foldl == sum