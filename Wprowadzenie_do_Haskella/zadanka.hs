-- Zadanie 3
euler :: Integer -> Integer
euler n = length
    [a | a <- [1..n], gcd a n == 1]

sumEuler :: Integer -> Integer
sumEuler n = sum [euler k | k <- [1..n], mod n k == 0]

-- Zadanie 4
pitagoras :: Integer -> Integer
pitagoras n = length
    [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2 && gcd b c == 1]

-- Zadanie 5
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1) + fib (n-2))

fib2 :: Integer -> Integer
fib2 n 
    | n == 1 = 1
    | n == 2 = 1
    | n > 2 = fib2 (n-1) + fib2 (n-2)

-- Zadanie 6
choose :: Integer -> Integer -> Integer
choose n k
    | k == 0 || k == n = 1
    | otherwise = choose (n-1) (k-1) + choose (n-1) k

-- Zadanie 7
perfect :: Integer -> [Integer]
perfect n =
    [k | k <- [1..n], sum [d | d <- [1..k], d /= k, mod k d == 0] == k]
