module StawyCLI where
import Control.Monad(when, void)
import Control.Concurrent.MVar
import Control.Concurrent(killThread, forkIO, threadDelay, yield)
import Data.Maybe(isJust)
import System.Environment(getArgs)
import System.IO(hPutStr, hPutStrLn, stderr)
import Data.List (sortBy)

data Pole = W | S | G deriving (Eq,Read)
instance Show Pole where
  show p 
    | p == W = " "
    | p == S = "0"
    | p == G = "*"

data Plansza = Pl {m::Int, n::Int, plansza :: [[Pole]]} deriving (Eq,Read)
instance Show Plansza where
  show (Pl x y p) = show x ++ " " ++ show y ++ "\n" ++ concatMap (\l -> concatMap show l ++ "\n") p
newtype Rozwiazanie = Rozwiazanie {planszar :: Plansza} deriving (Show)

main = do { 
  args <- getArgs;
  c <- if length args ==  0 then hPutStrLn stderr "Podaj nazwe pliku z zagadka." >> getLine else return $ args !! 0;
  stawy c; }

stawy :: FilePath -> IO ()
stawy file = do 
  hPutStrLn stderr $ "Rozwiazywanie zagadki z pliku: " ++ file 
  content <- readFile file
  let wdane = wczytajDane content
  clock <- newEmptyMVar
--aby umozliwic wypisywanie '.' co pewien krok w programie i zadbanie, by nie bylo to zbyt czeste uzylem zegara (dodatkowego watku ktory odswieza flage pozwalajaca cos wypisac) z taktowanie 100000 mikrosekund
  thrid <- forkIO (refresh clock)
--cala prace wykonuje funkcja rozwiaz
  rozwiazanie <- rozwiaz (notify (return ()) clock) wdane
  hPutStrLn stderr ""
  putStrLn $ concatMap (++"\n") $ map (ladnieFormatuj (doWstawienia wdane)) $ rozwiazanie
  killThread thrid

--funkcje wspolpracujace dla uzyskania wspomianego wyzej efektu zegara: notify wywoluje swoja f jesli tylko na clock'u jest odpowiednia flaga
notify f clock = do
  val <- tryTakeMVar clock
  when (isJust val) f

--a refresh odswieza wyzej wspomniana flage
refresh clock = do
  threadDelay 100000
  void $ tryTakeMVar clock
  putMVar clock ()
  yield
  refresh clock

--wymagane jest, aby w rozwiazaniu liczby staly na swoich miejscach, podczas gdy nie jest to istotne dla funkcji rozwiaz: tak wiec przed wypisaniem trzeba je wstawic
ladnieFormatuj :: [(Int,Int,Int)] -> Rozwiazanie -> String
ladnieFormatuj wsps r = concatMap (\(a,b) -> concatMap (\(c,d) -> let w = znajdzW (c,a) in if w == -1 then (show d) else (show w)) (zip [1,2..] b) ++ "\n") (zip [1,2..] $ plansza $ planszar r)
  where 
    znajdzW (x,y) = let cs = filter (\(a,b,c) -> a == x && b==y) wsps in if cs /= [] then thrd $ head cs else -1
    thrd (_,_,a) = a

--czesciowe rozwiazanie to takie, do ktorego nie wstawilismy jeszcze wszystkich stawow:
data CzescioweRozwiazanie = CR {planszacr :: Plansza, doWstawienia::[(Int, Int, Int)]} deriving (Show)
--ta funkcja wykonuje wiekszosc pracy programu; dziala symulujac nawroty prologa przez listy rozwiazan i concatMap. Dla danego czesciowego rozwiazania zwraca liste rozwiazan, co krok wywolujac funkcje notify
rozwiaz :: (IO ()) -> CzescioweRozwiazanie -> IO [Rozwiazanie]
rozwiaz notify cr = if (not . spojnaGrobla . kolorujNaCzarno) (planszacr cr) 
--do rozwiazania na pewno nie dojdziemy wstawiajac stawy do planszy na ktorej grobla juz nie jest spojna
  then return [] 
  else if ((doWstawienia cr) == []) 
--jesli juz nie ma kolejnego stawy do wstawienia to nalezy sprawdzic czy to co mamy jest dobrym rozwiazaniem i ewentualnie je zwrocic
    then return $ filter dobreRozwiazanie [Rozwiazanie $ kolorujNaCzarno $ planszacr cr]
--a wpp nalezy wygenerowac mozliwe wstawienia tego stawy i dla kazdego z nich wywolac rozwiaz 
    else do { notify; rss <- sequence $ map (rozwiaz notify) $ wstawStaw cr; return $ concat rss;}

--daneDFS zapamietuja jak aktualnie wyglada plansza, jakie jej pola juz odwiedzilismy oraz jakie nalezy odwiedzic. inf to dodatkowe informacje (jak np ile pol jeszcze nalezy wstawic/gdzie juz wstawilismy staw)
data DaneDFS a = DDFS { dane :: Plansza, odwiedzone :: [(Int,Int)], doOdwiedzenia :: [(Int,Int)], inf :: a} deriving (Show,Eq)
--mozliwe wstawienia stawy mozna generowac przez DFSa ktory pamieta, gdzie juz nie nalezy wstawiac pola ze stawem
--w inf zapamietuje ile jeszcze pol powinien miec staw oraz na jakich polach juz jest
wstawStaw :: CzescioweRozwiazanie -> [CzescioweRozwiazanie]
--wybieramy pierwszy staw do wstawienia, wywolujemy funkcje aux ktora zwroci wszystkie mozliwe ustawienia stawu do zadanej ilosci pol, filtrujemy z tego stawy ktore maja dokladnie tyle pol ile chcemy, nastepnie zamalowujemy na planszy jego pola
wstawStaw cr = (let (a,k) = headNRest $ doWstawienia cr in [CR (zamalujPola (dane x) (snd $ inf x) S) k | x <- aux a, (fst $ inf x) == 0])
 where
--staw nie moze sie stykac z innymi (sprawdzamy sasiadow pola)
 moznaWstawicStaw plan sasiedzipola = (filter (\y -> (znajdzPole plan y) == S) sasiedzipola) == []
 aux1 l = let (a,b) = splitBy (\x-> (doOdwiedzenia x) ==  [] || (fst$inf x) == 0) l in a ++ (concatMap (aux1.rozwin) b) 
 aux (a,b,c) =  aux1 [(DDFS (planszacr cr) [] [(a,b)] (c,[]))]
 rozwin x = ( let (cur, rest) = headNRest $ doOdwiedzenia x in 
--wybieramy, ze w to pole nie wstawiamy stawu (zaznaczamy je tylko jako odwiedzone)
  [x {odwiedzone = cur : (odwiedzone x), doOdwiedzenia = rest}] ++  
  let sasiedzic = sasiedzi cur (dane x) 
--nastepnie sprawdzamy czy w to pole mozna wstawic staw (nie bylo juz odwiedzone i nie sasiaduje z innym stawem)
    in if (cur `elem` (odwiedzone x)) || (not $ moznaWstawicStaw (dane x) sasiedzic) then [] 
--jesli tak, to dokladamy to pole do odwiedzonych, w informacjach zapamietujemy gdzie wstawilismy oraz to, ze chcemy juz wstawic o 1 mniej pol, do kolejki dorzucamy sasiadow tego pola
      else [x {odwiedzone = cur : (odwiedzone x), doOdwiedzenia = sasiedzic ++ rest, inf = let nr = fst $ inf x; gs = snd $ inf x in (nr-1, cur:gs)}])

dobreRozwiazanie :: Rozwiazanie -> Bool
--dobre rozwiazanie to takie w ktorym nie ma na grobli kwadratow i jest ona spojna
dobreRozwiazanie r = let nr = (planszar r) in (nieMaCzarnychKwadratow nr) && (spojnaGrobla nr)
--dobreRozwiazanie _ = True

--brak kwadratow sprawdzamy przez stworzenie wszystkich mozliwych kwadratow i sprawdzenie czy ktorys z nich jest czarny
nieMaCzarnychKwadratow :: Plansza -> Bool
nieMaCzarnychKwadratow r = let p = plansza r in 
  let parywierszy = zip p (tail p) in   
    and $ map (\(a,b) -> let kwadraty = let pw = zip a b in zip pw (tail pw)
      in and $ map (\((a,b),(c,d)) -> not (a == G && b == G && c == G && d == G)) kwadraty) parywierszy 

--z kolej spojnosc zapewniemay przez sprawdzenie czy nie ma wolnych pol (poniewaz malowanie grobli odbywa sie przez DFS, to brak jej spojnosci oznaczalby ze jakiegos pola nie pokolorowalismy)
spojnaGrobla :: Plansza -> Bool
spojnaGrobla p = nieMaWolnego $ plansza p 
  where
  nieMaWolnego = and . map (\wiersz -> (filter (==W) wiersz) == []) 

sasiedzi :: (Int, Int) -> Plansza -> [(Int, Int)]
sasiedzi (a,b) pl = filter (naPlanszy pl) [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]
naPlanszy ::  Plansza -> (Int, Int) ->Bool
naPlanszy pl (a,b) = (a > 0 ) && (a <= (m pl)) && (b > 0 ) && ( b <= (n pl))

--ogolny schemat dfsa dzialajacego na wczesniej zadeklarowanych danychDFS
dfs :: ((Int,Int) -> a -> Plansza -> (a, Plansza, [(Int,Int)])) -> DaneDFS a -> Plansza
dfs f d =
  if doOdwiedzenia d == [] 
    then dane d 
    else let (x,xs) = headNRest $ doOdwiedzenia d 
      in let (apr, planszapr,dOpr) = if x `elem` (odwiedzone d) then (inf d, dane d, []) else  f x (inf d) (dane d) 
        in dfs f $ d { dane = planszapr, odwiedzone = x:(odwiedzone d), doOdwiedzenia = dOpr ++ xs, inf = apr }

headNRest :: [a] -> (a, [a])
headNRest (x:xs) = (x,xs)

--uzywa powyzszego dfsa by pokolorwac groble na czarno (dzieki temu latwo sprawdzic jej spojnosc)
kolorujNaCzarno :: Plansza -> Plansza
kolorujNaCzarno pl = dfs (\wsp _ pl -> if let k = znajdzPole pl wsp in k == G || k == W then ((), zamalujPole pl wsp G, sasiedzi wsp pl) else ((), pl, [])) $ DDFS pl [] (znajdzPierwszeCW pl) ()

--znajduje pole czarne lub wolne
znajdzPierwszeCW pl = case filter (\(a,b) -> b /= []) $ zip [1,2..] $ map (filter (\(_,b) -> b == G || b == W).( zip [1,2..])) $ plansza pl of
   [] -> []
   ((a,(b,_):_):_) -> [(b,a)]

--zamalowuje dane pole na planszy na dany kolor
zamalujPole :: Plansza -> (Int,Int) -> Pole -> Plansza
zamalujPole pl (m,n) p = pl {plansza = modT (modT (\_ -> p) (m-1)) (n-1) $ plansza pl }
--zamalowuje dana liste pol
zamalujPola :: Plansza -> [(Int,Int)] -> Pole -> Plansza
zamalujPola p l po = foldl (\x y -> zamalujPole x y po) p l

znajdzPole pl (m,n) = (plansza pl) !! (n-1) !! (m-1)

--modyfikuje w-ty element list l funkcja f
modT :: (a -> a) -> Int -> [a] -> [a]
modT f w l = let (h,(t:ts)) = splitAt w l in h ++ ((f t):ts) 

parsujDane =  (map (Prelude.map (\x -> if x `elem` "\n\r." then ' ' else x) )) . lines
wczytajDane :: String -> CzescioweRozwiazanie
wczytajDane str = let tok = parsujDane str in 
  let m = read (tok !! 0) :: Int; n = read (tok !! 1) :: Int; wsps = sortBy thrd $ map (\(a,b,c) -> (b,a,c) ) $ read (tok !! 2) :: [(Int,Int,Int)] in
    CR (pustaPl (n) (m)) wsps
  where 
    thrd (_,_,c) (_,_,c') = compare c  c'

gen :: a -> Int -> [a]
gen b i 
 | i==0 = []
 | otherwise = b:(gen b (i-1))

pustaPl m n = Pl m n (gen (gen W m) n)

splitBy f xs
  | xs == [] = ([],[])
  | otherwise = let (x:xsp) = xs in let (p,k) = splitBy f xsp in if f x then (x:p,k) else (p,x:k)


