import qualified Data.Set
import qualified Data.Map

class DFSInput (t a b) where
  next :: t a b -> [a]
  moveNext :: t a b -> t a b
  addNexts :: t a b -> [a] -> t a b
  isVisited :: t a b -> a -> Bool
  addVisited :: t a b -> a -> t a b
  getInfo :: t a b -> b
  setInfo :: t a b -> b -> t a b

class Board (t a b) where
  neighbours :: t a b -> a -> [a]
  getVal :: t a b -> a -> b
  setVal :: t a b -> a -> b -> t a b

--Free | Stream | Wall (numbered ones -> Walls)
data Tile = F | S | W

paintVals :: Board (t a b) => t a b -> (b -> Bool) -> b -> [a]  -> t a b
paintVals t f b = foldl (\tpr a -> if f $ getVal tpr a then setVal tpr a b else tpr) t

type DFSFunc t a b = DFSInput (t a b) => Bool -> t a b -> [t a b]

paintNeighbours :: Board (t a b) => Data.Set b -> b -> DFSFunc (p a (t a b))
paintNeighbours s b isNewV dfsi = if not isNewV 
  then [] 
  else let brd = getInfo dfsi; cur = next dfsi in if cur `Data.Set.member` s then [(flip addNexts) (neighbours brd cur) $ moveNext $ (flip addVisited) cur $ setInfo dfsi $ paintVals brd (\_ -> True) b $ neighbours brd cur]

dfsPaintNeigh Board (t a b) => t a b -> a -> Data.Set b -> b -> t a b
dfsPaintNeigh brd set strt clr = getInfo $ dfsRun [paintNeigh set b] strt

--assert $ next dfsi /= [] (every DFSFunc can work under this assumption)
dfsPropagate :: DFSInput (t a b) => [DFSFunc t a b] -> t a b -> [t a b]
dfsPropagate fs dfsi = if ifVisited dfsi (head $ next dfsi) then (moveNext dfsi):(concatMap (\f -> f False dfsi) fs) else concatMap (\f -> f True dfsi)

dfsLoop :: DFSInput (t a b) =>[DFSFunc t a b] -> [t a b] -> [t a b]
dfsLoop fs dfsis = let (nprops, props) = partition (\a -> next a == []) dfsis in nprops ++ concatMap (dfsLoop . dfsPropagate) props

dfsRun :: DFSInput (t a b) => [DFSFunc t a b] -> t a b -> [t a b]
dfsRun fs start = dfsLoop fs [start]

data DFSi a b = D {dNexts :: [a], dVisited :: Data.Set a, addInfo :: b} deriving (Show, Read)
instance DFSInput (DFSi a) where
  next d = take 1 $ dNexts d
  moveNext d = d { dNexts = drop 1 (dNexts d) }
  addNext d t = d { dNexts = dNexts d ++ t }
  isVisited d p = p `Data.Set.member` (dVisited d)
  addVisited d p = d { dVisited = p `Data.Set.insert` (dVisited d) }
  getInfo = addInfo
  setInfo d b = d { addInfo = b }

--assert $ forall w <- [1..width], h <-[1..height] (w,h) `in` board 
data Ord a => BoardIns a b = BI { width :: a, height :: a, board :: Data.Map (a,a) b}
instance Board (BoardIns a b) where
  
instance Board (BoardIns a b) where
  neighbours b (a,b) = filter (\(c,d) -> c <= width && d<=height && c > 0 && d > 0) [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]
  getVal b a = (board b) ! a
  setVal brd b a = brd { board = insert a b (board brd) }
