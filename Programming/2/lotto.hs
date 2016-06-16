import qualified Data.Set
import qualified Data.Map
import Numeric

lotto :: FilePath -> IO ()
lotto f = do
  c <- readFile f
  let c' = filter (/='.') c --pozbywanie sie prologowych kropek
  putStr $ concatMap (show) $ solve $ initSolData c'


solve :: SolData -> [SolData]
solve sd = aux sd (0,0) --uzywa akumulatora aby pamietac na jakim polu planszy sie aktualnie znajduje
  where
    aux sd (x,y) 
      | y >= (m sd) = [sd]
      | otherwise = let p =((x+1)`mod`(m sd), y+(x+1)`div`(m sd)) --przechodzi plansze z lewa na prawo, od gory do dolu
        in concatMap ((flip aux) p) $
          (if canBeDropped sd (x,y) then filter (noStrayParts) $  [dropTile sd (x,y)] else []) ++ --jesli pole mozna zaczernic dodaje taka mozliwosc do listy potencjalnych rozwiazan,  pozbywa sie rozwiazan w ktorych czarne pola odcinaja kawalek planszy (ten kawalek moze w kolejnych wywolaniach stac sie czarny, ale poniewaz czarne pola nie moga sasiadowac nie wplywa to na poprawnosc)
          (if canBeTaken sd (x,y) then [takeTile sd (x,y)] else []) --analogicznie jesli moze pozostac niezaczerniony

{--parsuje string (uzywajac lex) i tworzy z niego dane wejsciowe --}
initSolData :: String -> SolData
initSolData str = let (h,t) = head $ lex str; m' = read h :: Int; b' = read t :: [[Tile]] in SolData m' (replicate (m') Data.Set.empty) (replicate (m') Data.Set.empty) $ Board b'

{--reprezentacja stanu potencjalnego wyniku--}
data SolData = SolData {m :: Int, hor :: [Data.Set.Set Int], ver :: [Data.Set.Set Int], board :: Board} -- wymiary, zbior elementow dla kazdego wiersza i kolumny (horizontal - wiersz, vertical - kolumna) oraz sama plansza
instance Show SolData where
  show sd = (show $ board sd)

{--funkcja pomocnicza, zamienia y-ty element xs na f od niego, nic nie robi jesli indeks jest poza tablica--}
changeL :: (a->a) -> [a] -> Int -> [a]
changeL f xs y = snd $ foldl (\(c,b) n -> if c == y then (c+1, b++[f n]) else (c+1, b++[n])) (0,[]) xs

{--tworzy potencjalne rozwiazanie w ktorym zostal wziety element (x,y)--}
takeTile :: SolData -> (Int,Int) -> SolData
takeTile sd (x,y) = let Board brd = board sd; Num n = brd !! y !! x 
  in sd { hor = changeL (n `Data.Set.insert`) (hor sd) y, --wstawia odpowiedni element planszy do wlasciwego zbioru wierszy
          ver = changeL (n `Data.Set.insert`) (ver sd) x } --analogicznie dla kolumn

{--podobnie jak funkcja powyzsza, ale element (x,y) zostal zaczerniony--}
dropTile :: SolData -> (Int,Int) -> SolData
dropTile sd (x,y) = let brd = board sd in sd { board = setTile B brd (x,y) } --jedynie zaczernia dany element planszy

{--sprawdza czy element (x,y) moze pozostac niezasloniety w danym potencjalnym rozwiazaniu--}
canBeTaken :: SolData -> (Int,Int) -> Bool
canBeTaken sd p@(x,y) = let Num n = getNum (board sd) p in 
  (not $ n `Data.Set.member` (hor sd !! y)) && --sprawdza czy wystapil on juz w swoim wierszu
  (not $ n `Data.Set.member` ( ver sd !! x)) --i swojej kolumnie

{--jw, ale czy mozna go zaslonic--}
canBeDropped :: SolData -> (Int,Int) -> Bool
canBeDropped sd p@(x,y) = let Num n = getNum (board sd) p in noBlackNeighbours (board sd) (m sd) p --jedynym wymaganiem jest, aby nie sasiadowal z innymmi czarnymi polami, spojnosc jest sprawdzona juz po ewentualnym zaczernieniu

{--pomocnicza funkcja, sprawdza czy element nie ma czarnych sasiadow--}
noBlackNeighbours brd m p@(x,y) = let neigh = neighbours brd m p in not $ B `elem` (map (getNum brd) neigh)

{--sprawdza, czy na danej planszy elementy niezaczernione sa spojne--}
noStrayParts :: SolData -> Bool
noStrayParts sd = allNum0 $ dfsPaintNeighbours0 sd

{--pomocnicza funkcja, przechodzi po planszy dfs'em zamieniajac nieczarne pola na Num 0--}
dfsPaintNeighbours0 :: SolData -> SolData
dfsPaintNeighbours0 sd = let brd = board sd in aux sd Data.Set.empty [fst $ head $ filter (isNum.snd) $ map (\a -> (a, getNum brd a)) [(x,y) | x <- [0..m sd-1], y <-[0..m sd-1]]]
  where
    {--uzywa dwoch akumulatorow: zbioru do zapisu juz odwiedzonych elementow i listy jako stosu elementow do przejscia --}
    aux:: SolData -> Data.Set.Set (Int,Int) -> [(Int,Int)] -> SolData
    aux p _ [] = p
    aux p s (x:xs) = aux 
      (p { board = setTile (Num 0) (board p) x }) --odwiedzony element na liscie kolorujemy,
      (x `Data.Set.insert` s) --dorzucamy go do zbioru odwiedzonych (uzycie zbioru jest szybsze niz zagladanie do elementu tablicy)
      ((if x `Data.Set.member` s then [] else 
        (filter (isNum.(getNum (board p))) $ neighbours (board p) (m p) x)) ++ xs) --oraz dopisujemy jego sasiadow do oczekujacych na przejscie

{--pomocnicza funkcja, sprawdza czy wszystkie nieczarne elementy na planszy sa Num0 (czyli czy powyzsza funkcja przeszla je wszystkie, bo w rozwiazaniu wystapuja liczby od 1 do m)--}
allNum0 :: SolData -> Bool 
allNum0 sd = let brd = board sd in (filter (/=Num 0) $ filter (/=B) $ map (getNum brd) [(x,y) | x <- [0..m sd-1], y <- [0..m sd-1]]) == []

{--reprezentacja tablicy danych, uzywam newtype aby zdefiniowac wlasna instancje klasy Show--}
newtype Board = Board [[Tile]]

{--zwraca wartosc w (x,y), error jesli takiego elementu nie ma--}
getNum :: Board -> (Int,Int) -> Tile
getNum brd (x,y) = let Board b = brd in b !! y !! x

{--ustawia element (x,y) na odpowiednia wartosc, nie robi nic jesli indeksy poza tablica--}
setTile :: Tile -> Board -> (Int,Int) -> Board
setTile t b (x,y) = let Board brd = b in Board $ changeL (\l -> changeL (\_ -> t) l x) brd y 

{--zwraca sasiadow elementu (x,y), zawsze poda elementy lezaca faktycznie w tablicy o ile tylko istnieja takie (mozna podac argument spoza tablicy i zawsze otrzyma sie poprawny wynik)--}
neighbours :: Board -> Int -> (Int,Int) -> [(Int,Int)]
neighbours brd m (x,y) = (if x+1 < m then [(x+1,y)] else []) ++ (if x-1 >= 0 then [(x-1,y)] else []) ++ (if y+1 < m then [(x,y+1)] else []) ++ (if y-1 >= 0 then [(x,y-1)] else [])

{--na planszy wystepuja tylko pola czarne lub majace numer calkowity--}
data Tile = B | Num Int deriving (Eq)
isNum (Num _) = True
isNum _ = False

{--wczytuje Tile ze Stringa, uwaga: nie da sie wczytac wartosci B--}
instance Read Tile where
  readsPrec _ str = case readDec str of
    [] -> []
    (a,str'):_ -> [(Num a, str')]

instance Show Tile where
  show B = "* "
  show (Num n) = show n ++ " "

instance Show Board where
  show (Board b) = concatMap (\x -> auxmap x) $ show $ b
    where
    auxmap ']' = "\n"
    auxmap ',' = ""
    auxmap '[' = ""
    auxmap t   = [t]

