import Data.Char

-- quicksort
qs :: [Int] -> [Int]
qs [] = []
qs [x] = [x]
qs (x:xs) = (qs [t| t<- xs, t<= x]) ++ [x] ++ (qs [t| t<- xs, t>x])

-- kod cezara

letter2int :: Char -> Int
letter2int c = ord c - ord 'a'

int2Letter :: Int -> Char
int2Letter x = chr (x + ord 'a')

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2Letter ((letter2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n x = map (shift n) x

-- łamanie szyfru cezara

-- tablica czestotliwosci liter w ang
engLetterFreq = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, ]