{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- :set -fno-warn-missing-methods

data Zero
data Suc n
type Three = Suc (Suc (Suc Zero))

data M m ms = M m ms
data M' m = M' m

data NB = NB
data P1 = P1 
data P2 = P2 
data P3 = P3 
data P4 = P4

class ValidBoard b

class Move m
instance Move P1
instance Move P2
instance Move P3
instance Move P4

type Done = M P3 (M P2 (M P1 (NB)))
--use template haskell to generate all different types that are valid boards?

-- need unique type parameters for each move
instance ValidBoard (NB)
instance ValidBoard (M m1 NB)
--instance (Move m1, Move m2)=>ValidBoard  (M m2 (M m1 NB))
--instance forall m1 m2 b. (Move m2, ValidBoard (M m1 b)) => ValidBoard (M m2 (M m1 b))
--instance forall m2 m1 b. (ValidBoard (M m1 b)) => ValidBoard (M m2 (M m1 b))
-- runST :: forall a. (forall s. ST s a) -> a
--instance forall m2.Move m2 => ValidBoard (M m2 (forall m1. M m1 ))


doneT = M P3 (M P2 (M P1 (NB)))
--m2T =  P2 (M P1 (NB)))
--doubleMoveT = move P1 (M P1 (NB))

-- ? flexiblecontexts allows (M m b) as type - ie data constructor in there?
move :: (Move m, ValidBoard b, ValidBoard (M m b)) => m->b->(M m b)
move p b = M p b

--move' :: forall m2. (forall m1. M' m1) -> m2
--move' m = m 


data True
data False

class Even n b where
  even :: n -> b
class Odd n b where
  odd :: n -> b

instance Even Zero True
instance Odd n b => Even (Suc n) b
instance Odd Zero False
instance Even n b => Odd (Suc n) b
