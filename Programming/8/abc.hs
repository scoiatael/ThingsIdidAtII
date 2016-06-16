import qualified Data.Map
import System.Environment (getArgs)


main = do
  sciezka <- getArgs
  if 0 == length sciezka then getLine >>= abc else abc $ sciezka !! 0

abc :: FilePath -> IO ()
abc f = do
  dane <- readFile f
--parseBoard rozpoznaje kolejne wartosci po nowych liniach, prologowe kropki usuwam (mozna by uzywac kropek do dzielenia na wartosci)
  print $ solve $ parseBoard $ filter( /= '.') dane

solve :: Board -> [Board]
solve b = map fst $ filter snd $ aux [(b,False)] (0,1) (width b, height b)
  where
--aux wstawia litery do wszystkich pol od (w+1,h) do konca planszy we wszystkich mozliwosciach z listy
--najpierw wstawia do (w+1,h) potem wywoluje sie rekurencyjnie dla nowej listy i powiekszonych wspolrzednych
    aux l (w,h) (mxw, mxh) = if h > mxh then l else aux (concatMap ((flip auxCharInsert) (w+1,h)) l ) ((w+1) `mod` mxw, h+(w+1)`div`mxw) (mxw, mxh)

--wstawia znak do planszy - dodatkowa zmienna boolowska stwierdza czy zapewniony jest warunek ze co najmniej jedna kolumna lub wiersz zawiera identyczne znaki (dzieki temu wystarczy na koniec odrzucic te rozwiazania, w ktorych ta zmienna jest falszywa)
auxCharInsert :: (Board, Bool) -> (Int,Int) -> [(Board,Bool)]
auxCharInsert (b,t) p@(x,y) = if isOccupied b p 
  then check b  
  else concatMap aux $ charlist $ maxchar b
  where
--tutaj aux wstawia konkretny znak: jesli to mozliwe zwraca liste jednoelementowa, jesli nie to pusta
    aux :: Char -> [(Board,Bool)]
    aux c = let 
      b' = insertCharAt b c p in check b'
--sprawdzenia wykonuje na koncu kazdego wiersza i kolumny
    check b' = let 
      ty = if x == width b then checkHorizontal b' y else Just True; 
      tx = if y == height b then checkVertical b' x else Just True 
        in if tx == Just False || ty == Just False then [] else [(b', t || tx == Nothing || ty == Nothing)]

--lista dozwolonych znakow
charlist :: Char -> [Char]
charlist ch = map (toEnum) [fromEnum 'A' .. fromEnum ch]

data Board = Board { maxchar :: Char, height :: Int, width :: Int, horizontal :: [Data.Map.Map Char Int], vertical :: [Data.Map.Map Char Int], occupied::Data.Map.Map (Int,Int) Char}
instance Show Board where
  show b = concatMap (\h -> '\n':(concatMap (\w -> if (w,h) `Data.Map.member` (occupied b) then ((occupied b) Data.Map.! (w,h)):[] else "_" ) ) [1..width b]) [1..height b]

--sprawdza czy dane pole jest zajete
isOccupied :: Board -> (Int,Int) -> Bool
isOccupied b p = p `Data.Map.member` (occupied b)

--assert $ c <= maxchar b
--wstawia znak do tablicy dodatkowo dodajac go listy znakow wystepujacych w danym wierszu i kolumnie
insertCharAt :: Board -> Char -> (Int,Int) -> Board
insertCharAt b c (x,y) = addOccupied b { horizontal = modList (Data.Map.insertWith (+) c 1) (horizontal b) y, vertical = modList (Data.Map.insertWith (+) c 1) (vertical b) x} (x,y) c
  where
  addOccupied :: Board -> (Int,Int) -> Char -> Board
  addOccupied b p c = b { occupied = Data.Map.insert p c (occupied b)}

--sprawdza jakie znaki znajduja sie w zadanym obszarze planszy (domyslnie wierszu/kolumnie, ale mozna by stworzyc dodatkowe), wyniki:
--Nothin -> all one char, Just True -> same number of each, Just False -> just wrong
--Nothing - wszystkie znaki w danym wierszu/kolumnie sa identyczne / Just True - jest ich tyle samo / Just False - warunki zadania sa naruszone
--assert $ i <= height
auxBoardCheck :: (Board -> [Data.Map.Map Char Int]) -> Board -> Int -> Maybe Bool
auxBoardCheck f b i = let elems = Data.Map.elems (f b !! i) in 
  if elems == [] 
    then Just False
    else if auxl elems 
      then Nothing 
      else let (x:xs) = elems in Just $ snd $ foldl (\(a,b) c -> (a,b && (a==c))) (x,True) xs
auxl (x:xs) = if xs == [] then True else False
 
checkVertical = auxBoardCheck vertical
checkHorizontal = auxBoardCheck horizontal

--modyfikuje i-ty element list l przez nalozenie funkcji f
--assert $ i < length l
modList :: (a->a) -> [a] -> Int -> [a]
modList f l i = let (c,(x:xs)) = splitAt i l in c ++ (f x):xs 

parseBoard :: String -> Board
parseBoard str = let wrds = lines str in makeBoard (read $ wrds !! 0 :: Int, read $ wrds !! 1 :: Int, read $ wrds !! 2 :: Char, read $ (concat $ drop 3 wrds) :: [(Int,Int,Char)])

makeBoard :: (Int,Int,Char, [(Int,Int,Char)]) -> Board
makeBoard (h,w,mc, cs) = foldl (\brd (b,a,c) -> insertCharAt brd c (a,b)) (Board mc h w (replicate (h+1) Data.Map.empty) (replicate (w+1) Data.Map.empty) Data.Map.empty) cs
