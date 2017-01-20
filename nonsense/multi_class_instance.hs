{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ConstraintKinds #-}
import GHC.TypeLits


class Done a b c d e f
class XOccupied a
instance XOccupied X
data X = X 
data O = O
data E = E
data Draw = Draw

--instance (XOccupied a, XOccupied b, XOccupied c) => Done a b c d e f
--instance (XOccupied d, XOccupied e, XOccupied f) => Done a b c d e f

data m ++ ms = M m ms
type (<>) x y = (x <=? y) ~ False

instance Done (X++ms) (X++ms) (X++ms) a b c
instance Done a b c (X++ms) (X++s) (X++s)  
instance Done (O++ms) (O++ms) (O++ms) a b c

data Pos a b c d e f = Pos a b c d e f
start = Pos E E E E E E 
m1x = Pos X E E E E E 
m2x = Pos E X E E E E 
m3x = Pos E E X E E E 
m4x = Pos E E E X E E 
m6x = Pos E E E E E X

class Empty a
instance Empty E
instance Empty e => Empty (E++e)

class Valid a
instance Valid E
instance Empty e => Valid (X++e)
instance Empty e => Valid (O++e)
instance Valid v => Valid (E++v)

class XMove a
instance (Empty e) => XMove (X++e)
instance (XMove x) => XMove (E++x)

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
move :: (Valid (a++as), Valid (b++bs), Valid (c++cs), Valid (d++ds), Valid (e++es),Valid (f++fs)) => Pos a b c d e f -> Pos as bs cs ds es fs -> Pos (a++as) (b++bs) (c++cs) (d++ds) (e++es) (f++fs)
move m b = undefined 

-- testing
vm :: (Valid a) => a -> Bool
vm _ = True

m1 = move m1x start 
m2 = move m2x $ move m1x start
m3 = move m3x $ move m2x $ move m1x start

-- fails:
--mf = move m1x m1
