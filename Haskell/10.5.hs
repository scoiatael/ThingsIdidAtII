roots :: (Double,Double,Double) -> [Double]

roots (a,b,c) = case a of
	0 -> case b of
		0 -> []
		otherwise -> [-c/b]
	otherwise -> case ((b**2-4*a*c) `compare` 0) of
		LT -> []
		EQ -> [-b/(2*a)]
		GT -> [(-b + sqrt(b**2-4*a*c))/(2*a), (-b - sqrt(b**2-4*a*c))/(2*a)]
	
	
data Roots = No | One (Double) | Two (Double,Double) deriving Show


roots2 :: (Double,Double,Double) -> Roots

roots2 (a,b,c) = case a of
	0 -> case b of
		0 -> No
		otherwise -> One(-c/b)
	otherwise -> case ((b**2-4*a*c) `compare` 0) of
		LT -> No
		EQ -> One (-b/(2*a))
		GT -> Two ((-b + sqrt(b**2-4*a*c))/(2*a), (-b - sqrt(b**2-4*a*c))/(2*a))