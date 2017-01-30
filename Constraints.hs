{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraints where

import Board
import GHC.Exts 
import Data.Type.Equality

type (<>) x y = (x == y) ~ False

type ValidMerge a b c d e f g h i as bs cs ds es fs gs hs is = (Valid (a+>as), Valid (b+>bs), Valid (c+>cs), Valid (d+>ds), Valid (e+>es), Valid (f+>fs), Valid (g+>gs), Valid (h+>hs), Valid (i+>is))
type ValidPlayer p p' = p'<>p
type Finished a b c d e f g h i = (Winner a b c d e f g h i)

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

type NoWins a b c d e f g h i = (Res a b c N, Res d e f N, Res g h i N, Res a e i N, Res c e g N, Res a d g N, Res c f i N)
-- wanted this too, doesn't seem to get that sophisticated with constraint types
-- type Wins a b c d e f g h i r = (Res a b c r1, Res d e f r2, Res g h i r3, Res a e i r4, Res c e g r5, Res a d g r6, Res b e h r7, Res c f i r8, W r1 r2 r3 r4 r5 r6 r7 r8 r)

-- calculate winner from all 8 possibly winning triplets
class W a b c d e f g h wins | a b c d e f g h -> wins
instance {-# OVERLAPPING #-} W N N N N N N N N N
instance W w N N N N N N N w
instance W N w N N N N N N w
instance W N N w N N N N N w
instance W N N N w N N N N w
instance W N N N N w N N N w
instance W N N N N N w N N w
instance W N N N N N N w N w

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

-- work out winner of a triplet, or nobody
data N = N
type family Res a b c where
  Res X X X = X
  Res O O O = O
  Res a b c = N

-- delete from here down and replace with something smart
-- this is only still needed to determine a draw
class Winner a b c d e f g h i
instance Winner X X X d e f g h i  
instance Winner a b c X X X g h i
instance Winner a b c d e f X X X
instance Winner X b c X e f X h i 
instance Winner a X c d X f g X i 
instance Winner a b X d e X g h X 
instance Winner X b c d X f g h X 
instance Winner a b X d X f X h i 

instance Winner O O O d e f g h i
instance Winner a b c O O O g h i
instance Winner a b c d e f O O O
instance Winner O b c O e f O h i 
instance Winner a O c d O f g O i 
instance Winner a b O d e O g h O 
instance Winner O b c d O f g h O 
instance Winner a b O d O f O h i 
instance {-# OVERLAPPABLE #-} (a<>E,b<>E,c<>E,d<>E,e<>E,f<>E,g<>E,h<>E,i<>E) =>
  Winner a b c d e f g h i 

