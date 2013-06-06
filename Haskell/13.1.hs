insert x [] = [[x]]
insert x yt@(y:ys) = (x:yt):(map ((:) y) (insert x ys))

permi1 [] = [[]]
permi1 (x:xs) = concatMap (insert x) (permi1 xs)

permi2 [] = [[]]
permi2 (x:xs) = [ys | zs <- permi2 xs, ys <- insert x zs ]

permi3 [] = return []
permi3 (x:xs) = permi3 xs >>= insert x

permi3a [] = return []
permi3a (x:xs) = (do
	ys <- permi3 xs
	es <- insert x ys
	return es)
	