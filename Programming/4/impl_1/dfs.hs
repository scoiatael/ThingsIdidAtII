import Control.Monad(void)
import Data.List(nub)
data Pole = W | C | B deriving (Eq,Show,Read)
data Plansza = Pl {m::Int, n::Int, plansza :: [[Pole]]} deriving (Show, Read)
sasiedzi :: (Int, Int) -> Plansza -> [(Int, Int)]
sasiedzi (a,b) pl = filter (naPlanszy pl) [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]
naPlanszy ::  Plansza -> (Int, Int) ->Bool
naPlanszy pl (a,b) = (a >= 0 ) && (a < (m pl)) && (b >= 0 ) && ( b < (n pl))

data DaneDFS a = DDFS { dane :: Plansza, odwiedzone :: [(Int,Int)], doOdwiedzenia :: [(Int,Int)], inf :: a}

dfs :: ((Int,Int) -> a -> Plansza -> (a, Plansza, [(Int,Int)])) -> DaneDFS a -> Plansza
dfs f d =
  if doOdwiedzenia d == [] 
    then dane d 
    else let (x,xs) = headNRest $ doOdwiedzenia d 
      in let (apr, planszapr,dOpr) = if x `elem` (odwiedzone d) then (inf d, dane d, []) else  f x (inf d) (dane d) 
        in dfs f $ d { dane = planszapr, odwiedzone = x:(odwiedzone d), doOdwiedzenia = nub $ dOpr ++ xs, inf = apr }

headNRest :: [a] -> (a, [a])
headNRest (x:xs) = (x,xs)

kolorujNaCzarno :: Plansza -> Plansza
kolorujNaCzarno pl = dfs (\wsp _ pl -> if let k = znajdzPole pl wsp in k == C || k == W then ((), zamalujPole pl wsp C, sasiedzi wsp pl) else ((), pl, [])) $ DDFS pl [] (znajdzPierwszeC pl) ()


znajdzPierwszeC pl = case filter (\(a,b) -> b /= []) $ zip [0,1..] $ map (filter (\(_,b) -> b == C).( zip [0,1..])) $ plansza pl of
   [] -> []
   ((a,(b,_):_):_) -> [(b,a)]

zamalujPole :: Plansza -> (Int,Int) -> Pole -> Plansza
zamalujPole pl (m,n) p = pl {plansza = modT (modT (\_ -> p) m) n $ plansza pl }

znajdzPole pl (m,n) = (plansza pl) !! n !! m

modT :: (a -> a) -> Int -> [a] -> [a]
modT f w l = let (h,(t:ts)) = splitAt w l in h ++ ((f t):ts) 

