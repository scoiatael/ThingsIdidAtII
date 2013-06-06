import qualified List (init)
my_scanl f a = map(foldr f a).init