safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit [] = Nothing
safeInit [x] = Nothing
safeInit (x:xs) = Just ([x] ++ (init xs))

-- splitWith :: (a -> Bool) -> [a] -> [[a]]

asInt_fold :: String -> Int
asInt_fold xs = foldr step 0 xs
    where step acc (x:xs) = 
        let acc' = acc * 10 + digitToInt x
        in step acc' xs