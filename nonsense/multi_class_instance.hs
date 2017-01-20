{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--import GHC.TypeLits
import Data.Type.Equality

data X = X 
data O = O
data E = E
data Draw = Draw

-- todo need to add restriction in on previous player<>current player 
data (++) m ms = M m ms
type (<>) x y = (x == y) ~ False

data Pos a b c d e f = Pos a b c d e f
nb s = (Pos E E E E E E,s)
pos1 p = (Pos p E E E E E,p)
pos2 p = (Pos E p E E E E,p)
pos3 p = (Pos E E p E E E,p)
pos4 p = (Pos E E E p E E,p)
pos5 p = (Pos E E E E p E,p)
pos6 p = (Pos E E E E E p,p)  

{-- use type family?
newBoard starter = case (starter) of
  X -> nb
  O -> nb 
--}

class Empty a
instance Empty E
instance Empty e => Empty (E++e)

class Valid a
instance Valid E
instance Empty e => Valid (X++e)
instance Empty e => Valid (O++e)
instance Valid v => Valid (E++v)

class XMove a
instance XMove (X++e)
instance (XMove x) => XMove (E++x)

class Done a b c d e f
instance Done (X++ms) (X++ms) (X++ms) a b c
instance Done a b c (X++ms) (X++s) (X++s)  
instance Done (O++ms) (O++ms) (O++ms) a b c

class Wins s w | s->w
--instance (XMove a) => Wins (Pos a b c d e f)
--instance Wins (Pos (X a) b c d e f) X
--instance Wins (Pos (a' (X a)) b c d e f) X
--instance Wins (Pos a (X b) c d e f) X
--instance Wins (Pos a (b' (X b)) c d e f) X
-- need this for draws
-- instance (Valid a, b, c... etc) => Wins (Pos a b c d e f) Draw 

whoWon :: Wins a b => a -> b
whoWon a = undefined 

--(Wins (Pos as bs cs ds es fs)~False)) = 
move :: (Valid (a++as), Valid (b++bs), Valid (c++cs), Valid (d++ds), Valid (e++es), Valid (f++fs), p'<>p) => ((Pos a b c d e f),p') -> ((Pos as bs cs ds es fs),p) -> (Pos (a++as) (b++bs) (c++cs) (d++ds) (e++es) (f++fs),p')
move m b = undefined 

-- testing
vm :: (Valid a) => a -> Bool
vm _ = True

b1 = move (pos1 X) (nb O) 
b2 = move (pos2 O) $ move (pos1 X) (nb O)
b3 = move (pos3 X) $ move (pos2 O) $ move (pos1 X) (nb O)

-- fails:
--mf = move pos1x pos1
--mf2 = move (pos2 X) $ move (pos1 X) (nb O) 

