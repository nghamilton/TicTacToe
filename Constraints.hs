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

type (<>) x y = (x == y) ~ False

-- Constraints for simplification
type IsValidMerge a b c d e f g h i as bs cs ds es fs gs hs is = (Valid (a+>as), Valid (b+>bs), Valid (c+>cs), Valid (d+>ds), Valid (e+>es), Valid (f+>fs), Valid (g+>gs), Valid (h+>hs), Valid (i+>is))
type IsValidPlayer p p' = p'<>p
type HasNoWins a b c d e f g h i = ((Winner a b c d e f g h i)==N) ~True
type HasWins a b c d e f g h i = (Winner a b c d e f g h i)<>N

-- non-result (i.e no win)
data N = N

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

-- something like this would have made life a lot easier .. but results in fundep conflicts, of course
--data NoDraw
--class Drawz a b c d e f g h i result | a b c d e f g h i -> result
--instance (a<>E,b<>E,c<>E,d<>E,e<>E,f<>E,g<>E,h<>E,i<>E) => Drawz a b c d e f g h i Draw 
--instance Drawz a b c d e f g h i NoDraw 
-- .. and then use something like this:
-- ... Draw a b c d e f g h i draw, DrawOrWin draw win result => (Board as bs cs ds es fs gs hs is, p) -> result

class Result a where
  get :: a
instance Result X where
  get = X
instance Result O where
  get = O 
instance Result N where
  get = N 

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

