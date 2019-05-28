main = do
  putStr "Podaj liczbe calkowita: "
  str <- getLine
  let n = read str :: Int
  putStr ("Jej szescian to " ++ show(n * n *n))
