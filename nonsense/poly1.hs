{-# LANGUAGE AllowAmbiguousTypes #-}

--m is inferred based on use of f
f::Monad m=>a->m a 
f a = return a 

class Board s where
  y :: Board x=>Int->x
  --t :: s

instance Board OkBoard where
  --t = Ok 
  y 1 = Ok
  y 2 = No 

instance Board NoBoard where
  --t = No
  y _ = No
 
data NoBoard = No 
 deriving (Show)
data OkBoard = Ok
 deriving (Show)

m::Board s=>OkBoard->Int->s
m b p = y p

move :: OkBoard -> String
move b = "yay"

--this works:
test = move (m Ok 1)

--This does not:
--move (m No 1)
