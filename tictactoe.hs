import Data.Array
import Control.Monad

type Magic = Int
data Player = Empty | X | O 
  deriving (Show,Eq)
type Move = Int
type MovePlayer = (Move,Player) 

type Pos = (Int,Int)
type Play = (Magic,Player)
type BState = Array Pos Play 
type PosState = (Pos,Play)
data Board a = Unplayable 
           | Playable {state :: BState}
           | XWon {state :: BState}
           | OWon {state :: BState}
           | Drawed {state :: BState}
  deriving (Show,Eq)

type NBoard = Board Player

maxsize = 3
empty = (0,Empty)
validMoves = [1..maxsize]

instance Monad Board where
  return p = newBoard 
  b >>= f = undefined 

{-- 

move m b = b >>= (validPlay m) >>= wins 
--doPlay takes a move and a board, gives a Unplayable or Playable
--checkWin takes a board and gives a Playable or Finished 
--board context records who started the play (therefore only need list of places)
--}
 
--given a position and a board, return a new board
move :: MovePlayer -> NBoard -> NBoard
move m b@(Playable a) = checkWin (doPlay m b)

doPlay :: MovePlayer -> NBoard -> NBoard 
doPlay (m,p) b@(Playable s) = case (playerAt s pos) of
  Empty -> Playable $ s // [(pos,(magicLookup m,p))]
  _     -> Unplayable
  where
    pos = posLookup m 

checkWin :: NBoard -> NBoard
checkWin b@(Playable s)
  | length (moves b) == (maxsize^2) = Drawed s 
  | otherwise = case (whoWon s) of
      Just X -> XWon s
      Just O -> OWon s
      Nothing -> b 

moves :: NBoard -> [Play]
moves = (filter (\(x,_)->x>0)).elems.state

playerAt :: BState -> Pos -> Player
playerAt s p  = snd $ s!p 

--newBoard :: NBoard
newBoard = Playable $ newState [((x,y),empty) | x<-[1..maxsize], y<-[1..maxsize]]

newState :: [PosState]->BState
newState s = array ((1,1),(maxsize,maxsize)) s 

posLookup :: Move -> Pos
posLookup i = indices (newState [])!!(i-1)

magicLookup:: Move -> Magic 
magicLookup i = [8,1,6,3,5,7,4,9,2]!!(i-1)

--reduce this down
whoWon :: BState -> Maybe Player
whoWon s 
  | won (plays X) = Just X 
  | won (plays O) = Just O 
  | otherwise = Nothing where
    won = elem 15 . map sum . filter (\x->3==length x) . filterM (\x -> [True, False]) 
    plays x = map fst $ filter (\a->x==snd a) $ elems s

--hasWon :: BState -> Maybe Player
{--hasWon s = if (null winners)
  then Nothing
  else Just winners --Just $ head $ head winners
  where
    winners = filter winner plays
    winner [(Player x),(Player x'),(Player x'')] = x==x'&&x'==x''
    winner _ = False
    plays = map convert winningCombos 
    convert = map ((playerAt s).posLookup)


winningCombos = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]
--} 

finishedT =  move (7,X) $ move (5,O) $ move (4,X) $ move (2,O) $ move (1,X) newBoard
drawedT = move (1,O) $ move (7,X) $ move (3,O) $ move (4,X) $ move (6,O) $ move (2,X) $ move (8,O) $ move (9,X) $ move (5,O) newBoard 
invalidPlayT = move (5,X) $ move (5,O) newBoard
