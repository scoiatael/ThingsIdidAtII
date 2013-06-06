data Term sig var = Var var | FunSym sig [Term sig var]

instance Monad (Term sig) where
	--return :: a -> Term sig a
	return = Var
	
	--(>>=) :: Term sig a -> (a -> Term sig b) -> Term sig b
	Var x >>= f = f x
	FunSym sig tdict >>= f = FunSym sig (map ( (flip (>>=)) f) tdict)