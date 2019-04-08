-- Zadanie 40
plus a b = (\u -> (\v -> u + v)) a b

-- Zadanie 41
ff = (2 ^)
gg = (^ 2)

-- Zadanie 42
mapgg n = map gg [1..n]
mapff n = map ff [1..n]

-- Zadanie 43
f' x y = (\z -> (\q -> z + 2 * q)) (x * y)
f'' a b = \y -> a*b + 2*y

-- Zadanie 44
f = \x -> x * x -- = x^2
g = \y -> f (f y) -- = x^4
h = g . g -- = x^16
h' x = x^16

-- Zadanie 46
zad46  n = (head $ map (\x y -> (x * x) + (y * y) ) [2 ,3 ,4]) n
zad46' n = head ([\y -> 4 + y^2, \y -> 9 + y^2, \y -> 16 + y^2]) n
zad46'' n = (\y -> 4 + y^2) n