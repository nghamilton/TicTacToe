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

type family Res a b c where
  Res X X X = X
  Res O O O = O
  Res a b c = N

--class W a b c wins | a b c -> wins
--instance W N N N N
--instance W X X X X

type family T a b c d e f where
  T a b c d e f = W (Res a b c) (Res b c d) 

type family W a b where
  W N N = N 
  W N a = a
  W a N = a
 
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
