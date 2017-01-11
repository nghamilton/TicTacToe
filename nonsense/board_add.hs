{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
type TWO = (S 2)

data Board p1 p2 p3 p4 p5 p6 p7 p8 p9 where
  NewBoard :: Board E E E E E E E E E
  P1 :: Board M E E E E E E E E  
  P2 :: Board E M E E E E E E E  
  P3 :: Board E E M E E E E E E  
  P4 :: Board E E E M E E E E E  
  P5 :: Board E E E E M E E E E  
  P6 :: Board E E E E E M E E E  
  P7 :: Board E E E E E E M E E  
  P8 :: Board E E E E E E E M E  
  P9 :: Board E E E E E E E E M  

data Player = X | O
data Status = XWon|OWon|Draw|Unfinished
data BoardState p1 p2 p3 p4 p5 p6 p7 p8 p9 = BoardState {
  status::Status,
  firstPlayer::Player,
--  moves::[Int],
  board :: Board p1 p2 p3 p4 p5 p6 p7 p8 p9}

type (<>) x y = (x <=? y) ~ False 

addBoard :: Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) -> Board (S p1') (S p2') (S p3') (S p4') (S p5') (S p6') (S p7') (S p8') (S p9') -> Board (S (p1+p1')) (S (p2+p2')) (S (p3+p3')) (S (p4+p4')) (S (p5+p5')) (S (p6+p6')) (S (p7+p7')) (S (p8+p8')) (S (p9+p9'))
addBoard _ _ = u 

-- move this into a constraint type
-- then make a Not for it, and use that for whoWon as a constraint
validBoard :: (p1<=1,p2<=1,p3<=1,p4<=1,p5<=1,p6<=1,p7<=1,p8<=1,p9<=1)=> Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) -> Board (S p1) (S p2) (S p3) (S p4) (S p5) (S p6) (S p7) (S p8) (S p9) 
validBoard = id

newGame p = BoardState Unfinished p NewBoard
move m b = validBoard $ addBoard m b
--whoWon :: (3<=(a+b+c))=> Board (M a) (M b) (M c) -> Bool 
--whoWon _ = True

{-- fails..

    • Couldn't match expected type ‘num0 ^ 2’
                  with actual type ‘(10 - 1000) * (10 - 1000)’
--}

sqr :: S num -> S (num * num)
sqr _ = GetState
root :: S (num^2) -> S num 
root _ = GetState
m10 :: S num -> S (num-10) 
m10 _ = GetState
f :: (num<=1) => S num -> S num
f _ = GetState

play :: S num -> S state -> S ((10^num)+state)
play _ _ = GetState

--abs :: S num -> S num
--abs a b = lg 
