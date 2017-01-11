{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ExistentialQuantification #-}

import GHC.TypeLits

u = undefined
data S (m :: Nat) = GetState 
type M = (S 1)
type E = (S 0)
type NotFinished = (S 0)
type Finished = (S 1)

empty = GetState :: E
notFinished = GetState :: NotFinished
finished = GetState :: Finished

data Board p1 p2 p3 p4 p5 p6 p7 p8 p9 m where
  NewBoard :: Board E E E E E E E E E (S 0)
  P1 :: Board M E E E E E E E E (S 1) 
  P2 :: Board E M E E E E E E E (S 2) 
  P3 :: Board E E M E E E E E E (S 3) 
  P4 :: Board E E E M E E E E E (S 4) 
  P5 :: Board E E E E M E E E E (S 5) 
  P6 :: Board E E E E E M E E E (S 6)  
  P7 :: Board E E E E E E M E E (S 7)   
  P8 :: Board E E E E E E E M E (S 8)   
  P9 :: Board E E E E E E E E M (S 9)    

data Player = X | O | Draw | Unfinished deriving (Show)
--data Winner = Player | Draw
--data Status = U 
data BoardState p1 p2 p3 p4 p5 p6 p7 p8 p9 ms status = BoardState {
  status::status,
  firstPlayer::Player,
  board :: Board p1 p2 p3 p4 p5 p6 p7 p8 p9 ms}

newGame p = BoardState notFinished p NewBoard

--data Board_ = forall (m :: Nat). Board_ (S m)
type (<>) x y = (x <=? y) ~ False 

addBoard :: Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) (S move) -> Board (S p1') (S p2') (S p3') (S p4') (S p5') (S p6') (S p7') (S p8') (S p9') (S moves) -> Board (S (p1+p1')) (S (p2+p2')) (S (p3+p3')) (S (p4+p4')) (S (p5+p5')) (S (p6+p6')) (S (p7+p7')) (S (p8+p8')) (S (p9+p9')) (S (move + (moves*10)))
addBoard _ _ = u 

-- move this into a constraint type
-- then make a Not for it, and use that for whoWon as a constraint
validBoard :: (p1<=1,p2<=1,p3<=1,p4<=1,p5<=1,p6<=1,p7<=1,p8<=1,p9<=1)=> Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) (S ms) -> Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) (S ms) 
validBoard = id

getMoves' :: (KnownNat ms) => Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) (S ms) -> S ms
getMoves' _ = GetState 
getMoves b = splitMvsVal $ fromInteger $ natVal $ getMoves' b
splitMvsVal :: Int -> [Int]
splitMvsVal 0 = []
splitMvsVal x = (splitMvsVal x')++[r] where
  (x',r) = x`divMod`10

move m (BoardState status p b) = BoardState status p b' where 
  b' = validBoard $ addBoard m b
  moves = (getMoves b') 

secondPlayer p = case (p) of
  X -> O
  O -> X 

whoWon (BoardState status p b)
  | won p1 = p 
  | won p2 = secondPlayer p
  | length moves == 9 = Draw
  | otherwise = Unfinished where
      won l = elem 15 $ [ x+y+z | x<-l, y<-l, z<-l,x/=y&&y/=z ]
      (p1,p2) = splitMoves moves
      moves = map (\x->[8,1,6,3,5,7,4,9,2]!!(x-1)) $ getMoves b

-- split up the move list into two sets of moves (for each player)
splitMoves :: [Int] -> ([Int],[Int])
splitMoves = foldl split ([],[]) where
  split (p1,p2) m = if (length p1<=length p2)
                      then (m:p1,p2)
                      else (p1,m:p2)


xwins = whoWon $ move P3 $ move P8 $ move P2 $ move P9 $ move P1 (newGame X)

-- should fail compile time
unfnsh = whoWon $ move P8 $ move P2 $ move P9 $ move P1 (newGame X) 


-- can never work because returns a different type for both cases
{--
changeStatus :: a -> S t1 -> S t2
changeStatus a status = case (a) of
  1 -> add status finished
  2 -> add status notFinished 

add :: (S s1)->(S s2)->(S (s1+s2))
add _ _ = GetState
--} 
