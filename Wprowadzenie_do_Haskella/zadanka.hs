-- Zadanie 3
euler :: Int -> Int
euler n = length
    [a | a <- [1..n], gcd a n == 1]

sumEuler :: Int -> Int
sumEuler n = sum [euler k | k <- [1..n], mod n k == 0]

-- Zadanie 4
pitagoras :: Int -> Int
pitagoras n = length [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2 && gcd b c == 1]

-- Zadanie 5
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1) + fib (n-2))

fib2 :: Integer -> Integer -- O (fib n)
fib2 n 
    | n == 1 = 1
    | n == 2 = 1
    | n > 2 = fib2 (n-1) + fib2 (n-2)

fib3 :: Integer -> Integer -- O (log^2 n)
fib3 0 = 0
fib3 1 = 1
fib3 n 
    | even n         = f1 * (f1 + 2 * f2)
    | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
    | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
    where 
        k = n `div` 2
        f1 = fib3 k
        f2 = fib3 (k-1)

fib4 n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

-- Zadanie 6
choose :: Integer -> Integer -> Integer
choose n k
    | k == 0 || k == n = 1
    | n > k = choose (n-1) (k-1) + choose (n-1) k
    | otherwise = error "k > n"

-- Zadanie 7
perfect :: Integer -> [Integer]
perfect n = [k | k <- [1..n], sum [d | d <- [1..k], d /= k, mod k d == 0] == k]

-- silnia
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

fac :: Int -> Integer
facs = scanl (*) 1 [1..]
fac n = facs !! n

fak :: Integer -> Integer
fak n = foldr (*) 1 [1..n]