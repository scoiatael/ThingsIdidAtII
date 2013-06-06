my_scanr f a [] = [a]
my_scanr f a (x:xs) = (foldr f a (x:xs)):(scanr f a xs)