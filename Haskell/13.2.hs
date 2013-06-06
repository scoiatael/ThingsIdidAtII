select:: [a] -> [(a,[a])]
select [x] = [(x, [])]
select (x:xs) = (x,xs):(map (\(y,ys) -> (y,x:ys)) (select xs))

perms1 [] = [[]]
perms1 xs = concatMap (\(y,ys) -> map (y:) (perms1 ys)) (select xs)

perms2 [] = [[]]
perms2 xs = [zs | ys <- select xs, zs <- (\(z,zs) -> map(z:) (perms2 zs)) ys ]

perms3 [] = return []
perms3 xs = select xs >>= (\(z,zs) -> map(z:) (perms3 zs))

perms3a [] = return []
perms3a xs = (do
	ys <- select xs
	es <- (\(z,zs) -> map(z:) (perms2 zs)) ys 
	return es)