class Monoid a where
	(***) :: a->a->a
	e::a
	infixl 6 ***
	
infix 7 ^^^
(^^^) :: Monoid a=> a-> Integer -> a
a^^^0=e
a^^^n = case n of
	0 -> e
	otherwise -> case n>0 of
		True -> case n`mod`2 of
			0 -> a^^^(n `div` 2) *** a^^^(n `div` 2)
			otherwise -> a***a^^^(n-1)
		otherwise -> e
		
		
instance Monoid Integer where
	(***) = (*)
	e = 1
	
instance Monoid Int where
	(***) = (+)
	e = 0
	
data Mtx2x2 a = Mtx2x2 a a a a deriving Show
data M2x2i = M2x2i Integer Integer Integer Integer deriving Show
instance Monoid (M2x2i) where
	(***) (M2x2i a11 a12 a21 a22) (M2x2i b11 b12 b21 b22) = M2x2i ((a11*b11)+(a21*b12)) ((a12*b11)+(a22*b12)) ((a11*b21)+(a21*b22)) ((a22*b22)+(a12*b21))
	e = M2x2i 1 0 0 1
	
my_trd :: M2x2i -> Integer
my_trd (M2x2i _ _ a _) = a

fibs n = my_trd ((M2x2i 0 1 1 1)^^^n)