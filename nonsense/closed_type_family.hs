{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Proxy
import GHC.TypeLits

data X = X
data O = O
data E = E
data N = N

data Result = InPlay | Draw

type family IsDraw a b where
  IsDraw E E = InPlay 
  IsDraw _ _ = Draw 

--t = undefined :: (IsDraw X X)

{--
λ: :kind! Or False (Int==Int)
Or False (Int==Int) :: Bool
= 'True
--}

-- Calculate any winners from all triplet combos, then check if any winners are not N (i.e. not a nobody)
type family Winner a b c d e f g h i where
  Winner a b c d e f g h i = W (Res a b c) (Res d e f) (Res g h i) (Res a e i) (Res c e g) (Res a d g) (Res b e h) (Res c f i) 

-- Check if any \= N - ie whether anybody has won
type family W a b c d e f g h where
  W N N N N N N N N = N
  W w N N N N N N N = w
  W N w N N N N N N = w
  W N N w N N N N N = w
  W N N N w N N N N = w
  W N N N N w N N N = w
  W N N N N N w N N = w
  W N N N N N N w N = w

-- work out winner of a triplet, or nobody
type family Res a b c where
  Res X X X = X
  Res O O O = O
  Res a b c = N


--f :: W a b c d => (a,b,c)->d
--f _ = undefined
--type family W a b c d e f where
--  W =  
type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or True  d     = True 
  Or f     True  = True 
  Or g     g     = g

type family And (a :: Bool) (b :: Bool) :: Bool where
  And False c     = False
  And True  d     = d
  And e     False = False
  And f     True  = f
  And g     g     = g
