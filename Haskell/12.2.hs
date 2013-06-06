ssm::Ord a => [a] -> [a]

ssm = foldr f [] where 
	f a [] = [a]
	f a xs = a:(filter ((<) a) xs)