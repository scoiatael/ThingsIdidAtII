import Prelude hiding (init)

-- 5:
newtype StateComput s a = SC( s -> (a,s))

instance Monad (StateComput s) where
	--return :: a -> StateComput s a
	return a = SC(\s -> (a,s))
	
	--(>>=) :: StateComput s a -> (a -> StateComput s b) -> StateComput s b
	SC m >>= f = SC(\s -> let (a, sp) = m s in 
		let SC g = f a in
			g sp)
			
-- 2:

-- Random = Num a => StateComput a

init :: Integer -> StateComput Integer ()
init a = SC(\_ -> ((), a))

random :: StateComput Integer Integer
random = SC(\seed ->
		let newSeed = 16807 * (seed `mod` 127773) - 2836 * (seed `div` 127773)
			in 
				if newSeed > 0 then (newSeed, newSeed) else 
					let newSeedT = newSeed + 2147483647
						in
							(newSeedT, newSeedT)
			)

randomInt :: Integer -> Integer
randomInt a = 
	let SC g = (do 
	{
		init a;
		x <- random;
		return x;
	})
	in fst $ g 1

-- 3:

-- SSC = StateComput String

runSSC :: StateComput String a -> String -> a
runSSC (SC f) str = fst $ f str

getc :: StateComput String Char
getc = SC(\s -> (head s, tail s))

ungetc :: Char -> StateComput String ()
ungetc c = SC(\s -> ((), c:s))

isEOS :: StateComput String Bool
isEOS = SC(\s -> (if s == [] then True else False, s))

countLines :: String -> Int
countLines = runSSC $ lines 0 where
	lines :: Int -> StateComput String Int
	lines n = do
		b <- isEOS
		if b
			then return n
			else do
				ch <- getc
				lines (if ch== '\n' then (n+1) else n)