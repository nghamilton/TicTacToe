{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE FlexibleContexts #-}


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

--{-# LANGUAGE DatatypeContexts #-}
--{-# LANGUAGE DataKinds #-}

import GHC.TypeLits

class Finished b -- (Board p1 p2 p3 p4 p5 p6 p7 p8 p9) -- winner where winner ::  (Board p1 p2 p3 p4 p5 p6 p7 p8 p9) -> winner

{-- 
Cant use negatives:
    • Illegal type synonym family application in instance:
        Board (0 - 1) (0 - 1) (0 - 1) p4 p5 p6 p7 p8 p9
--}

{--
    • No instance for (Finished (Board (1 + (1 + (1 + (1 + (0 - 1))))) 1 1 0 0 0 0 0 0
--}

instance Finished (Board 6 0 0 p4 p5 p6 p7 p8 p9 2)

--instance Finished (Board a b c 1 1 1 p7 p8 p9)
--instance Finished (Board a b c 2 2 2 p7 p8 p9)
--instance (Winner (a+b+c))=>Finished (Board a b c p4 p5 p6 p7 p8 p9)

{-- fails when attempting to match against whoWon $ addBoard (P3 x) $ addBoard (P2 x) $ addBoard (P1 o) (Start x) 
    • Couldn't match type ‘((0 - 1) + 1) + 1’ with ‘3’
... 
    • Couldn't match type ‘((1 + (1 + (0 - 1))) + 1) + 1’ with ‘3’

instance ((a+b+c)~3)=>Finished (Board a b c p4 p5 p6 p7 p8 p9 p)

--}

class Winner a b c 
--instance ((a+b+c)<=1)=>Winner a b c 
--instance ((a+b+c)<=2)=>Winner a b c 

 -- winner where winner x = undefined
--instance Finished 1 1 1 p4 p5 p6 p7 p8 p9 -- winner where winner x = undefined
--instance Finished p1 p2 p3 (S 1) (S 1) (S 1) p7 p8 p9 -- winner where winner x = undefined

{--

fails with     • Overlapping instances for Finished

instance ((a+b+c)~3)=>Finished (S a) (S b) (S c) p4 p5 p6 p7 p8 p9 -- winner where winner x = undefined
instance ((a+b+c)~3)=>Finished p1 p2 p3 (S a) (S b) (S c) p7 p8 p9 -- winner where winner x = undefined
instance ((a+b+c)~3)=>Finished p1 p2 p3 p4 p5 p6 (S a) (S b) (S c) -- winner where winner x = undefined

--}

data Player (n::Nat) = Get
type X = Player 1
type O = Player 2 
x = undefined :: X
o = undefined :: O 

data Board p1 p2 p3 p4 p5 p6 p7 p8 p9 p where
  Start :: Player s->Board 0 0 0 0 0 0 0 0 0 s 
  P1 :: Player s->Board 1 0 0 0 0 0 0 0 0 s 
  P2 :: Player s->Board 0 1 0 0 0 0 0 0 0 s 
  P3 :: Player s->Board 0 0 1 0 0 0 0 0 0 s 
  P4 :: Player s->Board 0 0 0 1 0 0 0 0 0 s 
  P5 :: Player s->Board 0 0 0 0 1 0 0 0 0 s 
  P6 :: Player s->Board 0 0 0 0 0 1 0 0 0 s 
  WON :: Player s->Board 1 1 0 0 0 0 0 0 0 s 
  WON2 :: Board 0 1 1 1 1 1 0 0 0 p 
  WON3 :: Board 0 0 0 0 0 0 1 1 1 p

{--
λ: whoWon $ addBoard Start WON 
True
it :: Bool
--}

whoWon :: (Finished (Board p1 p2 p3 p4 p5 p6 p7 p8 p9 p)) => Board p1 p2 p3 p4 p5 p6 p7 p8 p9 p-> Bool 
whoWon b = True


addBoard :: Board (p1) (p2) (p3) (p4) (p5) (p6) (p7) (p8) (p9) p -> Board (p1') (p2') (p3') (p4') (p5') (p6') (p7') (p8') (p9') p' -> Board (((p*p1)+p1')) ((p2+p2')) ((p3+p3')) ((p4+p4')) ((p5+p5')) ((p6+p6')) ((p7+p7')) ((p8+p8')) ((p9+p9')) (p)
 
addBoard _ _ = undefined 

