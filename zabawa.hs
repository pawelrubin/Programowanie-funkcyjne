
for 0 _ = return ()
for n doSth =
  do
   doSth
   for (n-1) doSth

head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  