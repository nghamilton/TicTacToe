{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

import Data.Array
import Control.Monad

type Magic = Int
data Player = Empty | X | O | Draw
  deriving (Show,Eq)
type Move = Int
type MovePlayer = (Move,Player) 

type Pos = (Int,Int)
type Play = (Magic,Player)
type BState = Array Pos Play 
type PosState = (Pos,Play)

data BoardS a = Unplayable 
           | Playable {state :: BState}
           | XWon {state :: BState}
           | OWon {state :: BState}
           | Drawed {state :: BState}
  deriving (Show,Eq)

type NBoard = BoardS Player

-- wrapper to hold moves
data NB = NewBoard Player
data B m = M Move m

-- all boards
class Board_ b where 
  extractMoves :: b -> [Int] 
  firstPlayer:: b -> Player 
  secondPlayer:: b -> Player 
  secondPlayer b = case (firstPlayer b) of
                     X -> O
                     O -> X

instance Board_ NB where
  extractMoves b = [] 
  firstPlayer (NewBoard p) = p 

instance Board_ b => Board_ (B b) where
  extractMoves (M m ms) = (extractMoves ms)++[m]
  firstPlayer (M m ms)  = firstPlayer ms 

-- valid boards
class Board_ b=>ValidBoard b
instance ValidBoard NB 
instance ValidBoard (B NB) 
instance ValidBoard (B (B NB)) 
instance ValidBoard (B (B (B NB))) 
instance ValidBoard (B (B (B (B NB)))) 
instance ValidBoard (B (B (B (B (B NB))))) 
instance ValidBoard (B (B (B (B (B (B NB)))))) 
instance ValidBoard (B (B (B (B (B (B (B NB))))))) 
instance ValidBoard (B (B (B (B (B (B (B (B NB)))))))) 
 
move :: ValidBoard b => Move -> b -> B b
move m b = M m b

--Calling on a game board that is empty or in-play is a compile-time type error
--not in class EmptyBoard or PlayingBoard
whoWon :: Board_ b=>b->Player
whoWon b
  | won p1 = firstPlayer b 
  | won p2 = secondPlayer b
  | otherwise = Draw
    where
      won l = elem 15 $ [ x+y+z | x<-l, y<-l, z<-l,x/=y&&y/=z ] 
      (p1,p2) = splitMoves moves 
      moves = map (\x->[8,1,6,3,5,7,4,9,2]!!(x-1)) $ extractMoves b

-- split up the move list into two sets of moves (for each player)
splitMoves :: [Int] -> ([Int],[Int]) 
splitMoves = foldl split ([],[]) where
  split (p1,p2) m = if (length p1<=length p2) 
                      then (m:p1,p2)
                      else (p1,m:p2)

{--
-- a unique type for each position
data P1
-- an InvalidBoard class that includes any Board with two of the same positions
class InvalidBoard
-- a ValidBoard class that inludes any board with 1-9 moves
class ValidBoard
-- so a type of P3 (P3 (P1 x)) is invalid
-- ... but (P2 (P1 (P4 x))) is valid
-- .. except this isn't possible to return different types, so ...

--the move function takes anything from the ValidBoard class
--and returns (a function that takes an int and returns a function 

-- i.e move -> pos -> board -> board
-- => apply pos to board, then move just returns the same board
-- ... has to be done this way to create an 'invalid' board that wont typecheck
--}

maxsize = 3
empty = (0,Empty)
validMoves = [1..maxsize]

{-- 
instance Monad Board where
  return p = newBoard 
  b >>= f = undefined 

move m b = b >>= (validPlay m) >>= wins 
--doPlay takes a move and a board, gives a Unplayable or Playable
--checkWin takes a board and gives a Playable or Finished 
--board context records who started the play (therefore only need list of places)
--}
 
--given a position and a board, return a new board
moveS :: MovePlayer -> NBoard -> NBoard
moveS m b@(Playable a) = checkWin (doPlay m b)

doPlay :: MovePlayer -> NBoard -> NBoard 
doPlay (m,p) b@(Playable s) = case (playerAt s pos) of
  Empty -> Playable $ s // [(pos,(magicLookup m,p))]
  _     -> Unplayable
  where
    pos = posLookup m 

checkWin :: NBoard -> NBoard
checkWin b@(Playable s)
  | length (moves b) == (maxsize^2) = Drawed s 
  | otherwise = case (whoWon' s) of
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
whoWon' :: BState -> Maybe Player
whoWon' s 
  | won (plays X) = Just X 
  | won (plays O) = Just O 
  | otherwise = Nothing where
-- put in do notation, use an Alternative guard?
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

--finishedT =  move (7,X) $ move (5,O) $ move (4,X) $ move (2,O) $ move (1,X) newBoard
--drawedT = move (1,O) $ move (7,X) $ move (3,O) $ move (4,X) $ move (6,O) $ move (2,X) $ move (8,O) $ move (9,X) $ move (5,O) newBoard 
--invalidPlayT = move (5,X) $ move (5,O) newBoard


--finishedT =  move 7 $ move 5 $ move 4 $ move 2 $ move 1 NewBoard
finishedT =  move 1 $ move 7 $ move 3 $ move 4 $ move 6 $ move 2 $ move 8 $ move 9 $ move 5 $ NewBoard X 
--badT = move 10 $ move 1 $ move 7 $ move 3 $ move 4 $ move 6 $ move 2 $ move 8 $ move 9 $ move 5 $ NewBoard X 

xWonBoardT =  M 7 $ M 5 $ M 4 $ M 2 $ M 1 $ NewBoard X 
yWonBoardT =  M 7 $ M 5 $ M 4 $ M 2 $ M 1 $ M 3 $ NewBoard X 
drawBoardT =  M 1 $ M 7 $ M 3 $ M 4 $ M 6 $ M 2 $ M 8 $ M 9 $ M 5 $ NewBoard X 
