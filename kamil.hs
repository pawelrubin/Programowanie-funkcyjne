euler :: Int -> Int
euler n = length [ a | a <- [2..n], gcd a n == 1]

eulerSum :: Int -> Int
eulerSum n = sum [euler k | k <- [1..n], mod n k == 0]

choose n k
  | k == 0 || k == n = 1
  | n > k = choose (n-1) (k-1) + choose (n-1) k
  | otherwise = error "k > n"


ecd [] = []
ecd [x] = [x]
ecd (x:xs)
  | x == (head xs) = ecd xs
  | otherwise = [x] ++ (ecd xs)
