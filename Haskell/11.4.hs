merge:: Ord a => ([a] , [a]) -> [a]

merge (x,[]) = x
merge ([],x) = x
merge ((x:xs),(y:ys)) = case x < y of
	True ->	x : merge (xs,(y:ys))
	otherwise ->	y:merge ((x:xs),ys)

msortn:: Ord a => [a] -> Int -> [a]

msortn _ 0 = []
msortn [x] 1 = [x]
msortn (x:xs) 1 = [x]
msortn x n = case n `mod` 2 of
	0 -> merge (msortn x (n `div` 2), msortn (drop (n `div` 2) x) (n `div` 2))
	otherwise -> merge (msortn x ((n-1) `div` 2), msortn (drop ((n-1) `div` 2) x) ((n+1) `div` 2))
	
	
merge_unique:: Ord a => [a] -> [a] -> [a]
merge_unique x [] = x
merge_unique [] x = x
merge_unique (x:xs) (y:ys) = case x==y of
	True -> x:(merge_unique xs ys)
	otherwise -> case x < y of
		True ->	x:(merge_unique xs (y:ys))
		otherwise ->	y:(merge_unique (x:xs) ys)
		
		
d235 :: [Int]
d235 = 1:((merge_unique [2*n | n <- d235]) $ merge_unique [3*n | n <- d235]  [5*n | n<-d235])
 