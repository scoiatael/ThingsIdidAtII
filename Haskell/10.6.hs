import qualified Char as Ch (intToDigit)
import qualified List as Ls (unfoldr)

sgn:: Integer -> String
sgn x = case x >= 0 of
	True -> ""
	False -> "-"

mabs:: Integer -> Integer
mabs x = case x >= 0 of
	True -> x
	False -> (-1*x)
	
mintegerToString:: Integer -> String
mintegerToString x = (sgn x) ++ (map Ch.intToDigit (reverse (Ls.unfoldr f (fromEnum (mabs x))))) where
	f 0 = Nothing
	f x =  Just (x `mod` 10, x `div` 10 )