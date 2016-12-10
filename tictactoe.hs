import Data.Array
import Control.Monad

type Move = Int
type Magic = Int
data Player = Empty | Player Char
  deriving (Show,Eq)

type Pos = (Int,Int)
type Play = (Magic,Player)
type BState = Array Pos Play 
type PosState = (Pos,Play)
data Board = Unplayable 
           | Playable {state :: BState}
           -- | Finished {state :: BState}
  deriving (Show,Eq)

type NBoard = Board

maxsize = 3
empty = (0,Empty)
validMoves = [1..maxsize]

{--instance Monad Board where
  return x = Playable [x]
  Playable a >>= f = return (f a)
  Unplayable >>= _ = Unplayable
--}
 
--given a position and a board, return a new board
move :: Move -> NBoard -> NBoard
--move f b = b >>= f
move p b@(Playable a) = if (validPlay p b)
  then Playable $ a // [((posLookup p),(magicLookup p,Player 'X'))]
  else Unplayable 

validPlay :: Move -> NBoard -> Bool
validPlay m b@(Playable s) = (playerAt s (posLookup m))==Empty

newBoard :: NBoard
newBoard = Playable $ newState [((x,y),empty) | x<-[1..maxsize], y<-[1..maxsize]]

newState :: [PosState]->BState
newState s = array ((1,1),(maxsize,maxsize)) s 

posLookup :: Move -> Pos
posLookup i = indices (newState [])!!(i-1)

magicLookup:: Move -> Magic 
magicLookup i = [8,1,6,3,5,7,4,9,2]!!(i-1)

--reduce this down
hasWon :: BState -> Maybe Player
hasWon s 
  | won (plays $ Player 'X') = Just $ Player 'X'
  | won (plays $ Player 'Y') = Just $ Player 'Y'
  | otherwise = Nothing where
    won = elem 15 . map sum . filterM (\x -> [True, False]) 
    plays x = map fst $ filter (\a->x==snd a) $ elems s

playerAt :: BState -> Pos -> Player
playerAt s p  = snd $ s!p 

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
