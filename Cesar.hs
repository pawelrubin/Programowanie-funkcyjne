import Data.Char
{-
  Przyklad wzorowany na rozdziale 5.5 ksiazki G. Huttona "Programming in Haskell"
  Jacek Cichon; WPPT PWr; 2019
-}

{-
  Przeksztalcanie liter z zakresu 'a' ..'z' na liczby
  z zakresu 0..25
-}
letter2Int :: Char -> Int
letter2Int c = ord c - ord 'a'

int2Letter:: Int -> Char
int2Letter x = chr (x + ord 'a')
 
shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2Letter ((letter2Int c + n) `mod`  26)
  | otherwise = c

-- Kod Cezara 
encode :: Int -> String -> String
encode n x = map (shift n) x

{-
Lamanie kodu Cesara
-}

-- tablica czestotliwosci liter w jezyku angielskim
engLetterFreq :: [Float ]
engLetterFreq = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n/fromIntegral m) * 100

-- ile razy wystepuje x w lancuchu xs
count :: (Eq a) => a -> [a] -> Int
count x xs =length [t | t<-xs, t == x]

-- czestotliwosc liter w tekscie
freq :: String ->[Float]
freq xs = [percent (count x xs) n | x<-['a'..'z']] where n = length xs

-- funkcja chi - squere
-- HINT: im mniejsza wartosc tym wieksze podobienstwo
chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [(obs-e)^2/e | (obs,e)<- zip xs ys]

-- np. rotate 2 [1,2,3,4] ==> [3,4,1,2]
rotate :: Int -> [a] ->[a]
rotate n xs = (drop n xs) ++ (take n xs)

-- na jakich pozycjach wystepuje element x w ciagu xs
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (i,y)<- zip [0..] xs, y==x]

-- funkcja do lamania kodu Cezara
crack :: String -> String
crack xs = encode (-guess) xs
  where
    guess = head ( positions (minimum chitab) chitab) 
    chitab =  [chisqr (rotate n frequency) engLetterFreq | n <- [0..26]]
    frequency = freq xs
