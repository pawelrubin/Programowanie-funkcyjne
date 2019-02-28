
myDrop n xs = if n <= 0 || null xs
    then xs
    else myDrop (n-1) (tail xs)


lastButOne :: [a] -> a
lastButOne xs
    | (length xs) > 1 = last (take ((length xs) - 1) xs)
    | otherwise = undefined
