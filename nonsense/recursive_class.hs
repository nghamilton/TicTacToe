{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}

data Player = Empty | X | O | Draw
  deriving (Show,Eq) 
type Move = Int      

data NB s = NewBoard s Player
data B s m = M s Move m

class State p where
  cs :: ValidBoard b => b -> (State s=>s)

instance State P where 
  cs b = getState b

instance State U where
  cs b = undefined 

data P = PlayableBoard
data U = UnplayableBoard

-- valid boards
class ValidBoard b
instance ValidBoard (NB P)
instance (ValidBoard b) => ValidBoard (B P b)

move :: (ValidBoard b, State s) => Move -> b -> B s b
move m b = M (cs b) m b 

getState :: (Board_ b, State s)=>b->s
getState b = case (whoWon b) of
           Draw -> PlayableBoard
           _ -> UnplayableBoard
 
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

class Board_ b where
  extractMoves :: b -> [Int]
  firstPlayer:: b -> Player
  secondPlayer:: b -> Player
  secondPlayer b = case (firstPlayer b) of
                     X -> O
                     O -> X

instance (State s)=>Board_ (NB s) where
  extractMoves b = []
  firstPlayer (NewBoard _ p) = p

instance Board_ b => Board_ (B s b) where
  extractMoves (M _ m ms) = (extractMoves ms)++[m]
  firstPlayer (M _ m ms)  = firstPlayer ms
