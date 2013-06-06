

newtype Fset a = Fset (a -> Bool)

empty :: Fset a
empty = Fset f where f _ = False

singleton :: Ord a => a -> Fset a
singleton a = Fset f where 
	f x = (x==a)

fromList :: Ord a => [a] -> Fset a
fromList list = Fset (f list) where
	f [] _ = False
	f (x:xs) a = (a==x) || (f xs a)  

	

member :: Ord a=> a -> Fset a -> Bool
member a (Fset f) = f a

union :: Ord a=> Fset a -> Fset a -> Fset a
union f1 f2 = Fset f where f a = (member a f1) || (member a f2)

intersection :: Ord a=> Fset a -> Fset a -> Fset a
intersection f1 f2 = Fset f where f a = (member a f1) && (member a f2)
