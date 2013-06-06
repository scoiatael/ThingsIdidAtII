my_lengthr = foldr (*) 0 where
	(*) _ n = n + 1
	
my_lengthl = foldl (*) 0 where
	(*) n _ = n + 1
	
my_app = flip $ foldr (:)

my_concat = foldr my_app []

my_reverse = foldl (flip (:)) []

my_sum = foldl (+) 0
	