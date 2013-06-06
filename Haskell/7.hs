data Nat = Zero | Succ Nat
(n+) , (n*) , (n^) :: Nat -> Nat -> Nat
m n+ Zero = m
m n+ Succ n = Succ (m n+ n)
m n* Zero = Zero
m n* Succ n = (m n* n) n+ m
m n^ Zero = Succ Zero
m n^ Succ n = (m n^ n) n* m