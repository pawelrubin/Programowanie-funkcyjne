mSqrt x = if x < 0 then Nothing else Just (sqrt x)

mInv x = if x == 0 then Nothing else Just (1/x)

lSqrt x 
  | x < 0 = []
  | x == 0 = [0]
  | otherwise = [sqrt x, -(sqrt x)]

lS2 a b d = if a == 0 then [] else [((-b) + d) / (2 * a)]

f x = ((Just x) >>= mSqrt) >>= mInv

lSolve2 a b c = (lSqrt (b*b-4*a*c)) >>= lS2 a b

