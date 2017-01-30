{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constraints where

import Board
import GHC.Exts 
import Data.Type.Equality
import Data.Type.Bool

type (<>) x y = (x == y) ~ False

-- Constraints for simplification
type IsValidMerge a b c d e f g h i as bs cs ds es fs gs hs is = (Valid (a+>as), Valid (b+>bs), Valid (c+>cs), Valid (d+>ds), Valid (e+>es), Valid (f+>fs), Valid (g+>gs), Valid (h+>hs), Valid (i+>is))
type IsValidPlayer p p' = p'<>p
type HasNoWins a b c d e f g h i = ( Winner a b c d e f g h i == N )~True
type HasWins a b c d e f g h i = Winner a b c d e f g h i <> N
type HasWinsOrDraw a b c d e f g h i = ( (Draws a b c d e f g h i) || (Winner a b c d e f g h i == X) || (Winner a b c d e f g h i == O) ) ~ True

-- Only a draw if no empty spots left
type family Draws a b c d e f g h i where
  Draws a b c d e f g h i = Not ((a==E) || (b==E) || (c==E) || (d==E) || (e==E) || (f==E) || (g==E) || (h==E) || (i==E)) 

-- non-result (i.e no win)
data N = N

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

-- an empty move 
class Empty a
instance Empty E
instance Empty e => Empty (E+>e)

-- an valid move - only empty moves before and after
class Valid a
instance Valid E
instance Empty e => Valid (X+>e)
instance Empty e => Valid (O+>e)
instance Valid v => Valid (E+>v)

class Result a where
  get :: a
instance Result X where
  get = X
instance Result O where
  get = O 
instance Result N where
  get = N 
