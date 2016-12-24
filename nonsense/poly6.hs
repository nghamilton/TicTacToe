{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, RankNTypes #-}
{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

import Data.Bits
class OK_ b
class Board_ b where
  extract :: Board -> b

instance Board_ OK where
  extract = undefined --(B b) = b

instance Board_ NO where
  extract = undefined --(B b) = b

instance OK_ OK

data OK = K deriving (Show)
data NO = N deriving (Show)
data Board = forall b. Board_ b => B b

move :: Bool -> Board 
move p = case p of 
  True -> B K 
  False -> B N 

--ext :: forall b. Board_ b => Board -> b
--ext b = extract b 

run :: Board OK -> Bool
run b = true
