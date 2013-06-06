merge:: Ord a => ([a] , [a]) -> [a]

merge (x,[]) = x
merge ([],x) = x
merge ((x:xs),(y:ys)) = case x < y of
	True ->	x : merge (xs,(y:ys))
	otherwise ->	y:merge ((x:xs),ys)

halve:: [a] -> ([a],[a])

halve ls = case ((length ls) `mod` 2) of
	0 -> (take ((length ls) `div` 2) ls, drop ((length ls) `div` 2) ls)
	otherwise -> ((take (((length ls)-1) `div` 2) ls), (drop (((length ls)-1) `div` 2) ls))
	

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = (merge . cross (msort,msort) . halve) xs
cross :: (a->c,b->d) -> (a,b) -> (c,d)
cross (f,g) = pair (f.fst, g.snd)
pair :: (a->b,a->c) -> a ->(b,c)
pair (f,g) x = (f x,g x)

