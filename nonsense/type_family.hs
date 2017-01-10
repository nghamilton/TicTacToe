{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}


data Player = Empty | X | O | Draw
  deriving (Show,Eq) 
type Move = Int      

data NB s = NewBoard s Player
data B s m = M s Move m

class State p
instance State (P s) 
instance State (U s)

data P b = PlayableBoard b deriving (Show)
data U b = UnplayableBoard b deriving (Show)

-- valid boards
class ValidBoard b where
  type BoardStatus b :: * -> *
  get :: b -> (BoardStatus b) String 

instance ValidBoard (NB s) where
  type BoardStatus (NB s) = P 
  get b = PlayableBoard "ok" 
 
instance (ValidBoard b) => ValidBoard (B s b) where
  type BoardStatus (B s b) = U 
  get b = UnplayableBoard "nope" 

playT = PlayableBoard "k" 
--moveT =  move 2 $ move 1 $ NewBoard playT X

--t :: B (P s) b -> Move
t x@(M (PlayableBoard s) m b) = get x 

--move m b = M (get b) m b
{--    • Could not deduce (State (BoardStatus b String))
        arising from a use of ‘M’
move :: (ValidBoard b) => Move -> b -> B s b
move m b = case m of
  1-> M s m b 
  2-> M s m b
  where
    s = get b 
--}

{-- getState :: (Board_ b, State s)=>b->s
getState b = case (whoWon b) of
           Draw -> PlayableBoard
           _ -> UnplayableBoard
--}
 
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

instance Board_ (NB s) where
  extractMoves b = []
  firstPlayer (NewBoard _ p) = p

instance Board_ b => Board_ (B s b) where
  extractMoves (M _ m ms) = (extractMoves ms)++[m]
  firstPlayer (M _ m ms)  = firstPlayer ms
