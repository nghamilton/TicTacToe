{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

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

instance {-# INCOHERENT #-} (a<>E,b<>E,c<>E,d<>E,e<>E,f<>E,g<>E,h<>E,i<>E) =>
  Winner a b c d e f g h i 

type Win a b c = (a~b, b~c, a<>E)

type NoWins a b c d e f g h i= (NoWin a b c, NoWin d e f, NoWin g h i, NoWin a e i, NoWin c e g, NoWin a d g, NoWin c f i)
class NoWin a b c
instance NoWin E E E
instance NoWin E E X
instance NoWin E E O
instance NoWin E X E
instance NoWin E X X
instance NoWin E X O
instance NoWin E O E
instance NoWin E O X
instance NoWin E O O
instance NoWin X E E
instance NoWin X E X
instance NoWin X E O
instance NoWin X X E
instance NoWin X X O
instance NoWin X O E
instance NoWin X O X
instance NoWin X O O
instance NoWin O E E
instance NoWin O E X
instance NoWin O E O
instance NoWin O X E
instance NoWin O X X
instance NoWin O X O
instance NoWin O O E
instance NoWin O O X

