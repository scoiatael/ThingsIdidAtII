fibs :: [Int]
fibs = 1:1:(zipWith (+) fibs (tail fibs))

member ::[a]->Int->a
member (x:_) 0 = x
member (_:xs) n = member xs (n-1)

--lepiej:
member :: [a] => Int -> Maybe a
member [] _ = Nothing
member (x:_) 0 = Just x
member (_:xs) n = member xs (n-1)