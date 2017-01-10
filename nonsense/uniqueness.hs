{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE ExistentialQuantification #-}

import GHC.TypeLits
import Data.Type.Equality 

type Not a b = ((b == a) ~ False) 
restrictChar :: Not Char a => a -> a 
restrictChar = id

data X = X deriving (Show) 
data O = O deriving (Show)
data NB = NB deriving (Show) 
data B m' b = forall m. B { getBoard :: m' -> (B m b) -> B m' (B m b) }
--data B m' b = forall m. B m' (B m b) 
--    MkAccum :: s -> (a -> s -> s) -> (s -> a) -> Accum a

class Board b
instance Board NB 
instance Board (B m s)
class Move m
instance Move X
instance Move O 

{-- given anything from the move class
 ..  and anything from the Board class that has a unique m
 .. return a new board with the current move, wrapping the old board --}
--move :: forall m' b. (Move m', Board b) => m' -> b -> B m' b
--move m b = B m b 

--(forall m. Board (B m b) => 

--t :: x ~ (B m b) => x -> m 
--t x = undefined 
