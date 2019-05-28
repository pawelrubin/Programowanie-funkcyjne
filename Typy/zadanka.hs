-- Zadanie 63
m91 n 
  | n > 100 = n - 10
  | otherwise = m91 (m91 (n + 11))

-- zadanie 64
data IntOrString = IntOrString Int String deriving Show

-- instance (Eq a) => Eq IntOrString

-- zadanie 65
data BTree a = L a | N (BTree a) a (BTree a) deriving Eq

-- zadanie 65.8
g z1 a z2 = (max z1 z2) + 1
foldTree f z (L a) = z
foldTree f z (N lt a rt) = f (foldTree f z lt) a (foldTree f z rt)

treeH = foldTree g 0
