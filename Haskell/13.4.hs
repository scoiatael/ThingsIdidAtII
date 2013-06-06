prod:: [Integer] -> Integer
-- Monad Maybe [Integer]
-- return [Integer] -> Maybe [Integer]
-- m >>= f = case m of Nothing fail Just c f c

f:: [Integer] -> Maybe Integer
f [x] = Just x
f (0:_) = Nothing
f (x:xs) = f xs >>= (\ y -> Just (x*y))
prod xs = case (Just xs) >>= f of 
	Just a -> a
	Nothing -> 0
	