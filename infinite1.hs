{-# LANGUAGE ExistentialQuantification #-}

data Brd = forall s. Show s => B (Int->Brd) s | forall s. Show s => N s 

y = B (f) 0

f :: Int -> Brd
f 1 = B (f) 0 
f 2 = N True 

-- fails as an approach because I can't differentiate in the type signature what type of Board it is that I want
