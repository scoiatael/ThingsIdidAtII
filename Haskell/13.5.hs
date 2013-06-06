data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList :: [a] -> Cyclist a
fromList (x:xs) = backward(aux (xs,[x])) where
aux:: ([a],[a]) -> Cyclist a
aux([x],[y]) = Elem (aux ([y],[x])) x (aux ([y],[x]))
aux ([x], y:ys) = Elem (aux (y:[x], ys)) x (aux (reverse (y:ys), [x]))
aux (x:xs, [y]) = Elem (aux ([y], reverse (x:xs))) x (aux (xs, x:[y]))
aux (x:xs, y:ys) = Elem (aux (y:x:xs, ys)) x (aux (xs, x:y:ys))

forward (Elem _ e1 (Elem l2 e2 r2)) = Elem l2 e2 r2
backward (Elem (Elem l2 e2 r2) e1 _) = Elem l2 e2 r2 
label (Elem _ e _ ) = e

enumInts = aux 0 where 
	aux n = Elem (aux (n-1)) n (aux (n+1))
	
newtype Cyc s a = Cyc (Cyclist s -> (a, Cyclist s))

instance Monad (Cyc s) where
	(>>=) (Cyc m) f = Cyc (\s ->
		let 
			(a,sp) = m s
		in
		(	let 
				Cyc g = f a
			in
				g sp
		))
	return a = Cyc (\s -> (a,s))
	
runCyc a (Cyc f) = fst (f a)
fwd = Cyc(\a -> ((), forward a))
bkw = Cyc(\a -> ((), backward a))
lbl = Cyc(\a -> (label a, a))

example = runCyc enumInts (do
	bkw
	bkw
	bkw
	bkw
	x <- lbl
	fwd 
	fwd 
	y <- lbl
	return (x+y))