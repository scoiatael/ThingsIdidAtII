sublist1 [] = [[]]
sublist1 (x:xs) = (map (x:) ys)++ys where ys = sublist1 xs

sublist1a [] = [[]]
sublist1a (x:xs) = concatMap (\c -> [x:c,c]) (sublist1a xs)

sublist2 [] = [[]]
sublist2 (x:xs) = [ys | zs <- sublist2 xs, ys <- (\c -> [x:c,c]) zs]

sublist3 [] = return []
sublist3 (x:xs) = sublist3 xs >>= (\c -> [x:c,c])

sublist3a [] = return []
sublist3a (x:xs) = (do
	zs <- sublist3a xs
	ys <- (\c -> [x:c,c]) zs
	return ys)