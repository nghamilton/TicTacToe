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
{-# LANGUAGE IncoherentInstances #-}

import Data.Type.Equality
--import Prelude hiding ((++))

data X = X deriving (Show) 
data O = O deriving (Show) 
data E = E deriving (Show)  
data Draw = Draw deriving (Show)  

infixr 5 ++ 
data (++) m ms = M m ms deriving (Show)  
type (<>) x y = (x == y) ~ False
--type Xs a b c = (X_ a, X_ b, X_ c)
--type Os a b c = (O_ a, O_ b, O_ c)
--type Es a b c d e f = (Empty a, Empty b, Empty c, Empty d, Empty e, Empty f)
--type E3s a b c = (Empty a, Empty b, Empty c)

data Pos a b c d e f = Pos a b c d e f deriving (Show)
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

-- an empty type
class Empty a
instance Empty E
instance Empty e => Empty (E++e)

-- an valid move - only empty moves before and after
class Valid a
instance Valid E
instance Empty e => Valid (X++e)
instance Empty e => Valid (O++e)
instance Valid v => Valid (E++v)

t :: X_ a b => a -> b 
t a = undefined

-- an x occupied type
class X_ a x | a -> x
instance X_ (X++e) X
instance (X_ x X) => X_ (E++x) X
-- an o occupied type
class O_ a
instance O_ (O++e)
instance (O_ o) => O_ (E++o)

class Done a b c d e f
instance Done X X X a b c
instance Done O O O a b c
instance Done a b c O O O
instance Done a b c X X X 
-- instance Done a b c d e f Draw
 
--instance (X_ m1, X_ m2, X_ m3) => Done m1 m2 m3 (a++as) (b++bs) (c++cs)
--instance (X_ m1, X_ m2, X_ m3) => Done (a++as) (b++bs) (c++cs) m1 m2 m3
--instance X_ m1, X_ m2, X_ m3 => Done (m1++X++ms1) (m2++X++ms2) (m3++X++ms3) a b c

--instance Es m1 ms1 m2 ms2 m3 ms3 => Done (m1++X++ms1) (m2++X++ms2) (m3++X++ms3) a b c
--instance E3s ms1 ms2 ms3 => Done (X++ms1) (X++ms2) (X++ms3) a b c

--instance (Empty a, Empty as) => Done m1 m2 m3 (a++X++as) (X++bs) (X++cs)
--instance Xs w1 w2 w3 => Done w1 w2 w3 (d++ds) (e++es) (f++fs)
--instance Xs w1 w2 w3 => Done (a++as) (b++bs) (c++cs) w1 w2 w3 
--instance Os w1 w2 w3 => Done w1 w2 w3 (d++ds) (e++es) (f++fs)

--instance (X_ d, X_ e, X_ f) => Done a b c d e f 
--instance (X_ d, X_ e, X_ f) => Done a b c (X++ms) (X++s) (X++s)  
--instance Done (O++ms) (O++ms) (O++ms) a b c

--class Wins s w | s->w
--instance (XMove a) => Wins (Pos a b c d e f)
--instance Wins (Pos (X a) b c d e f) X
--instance Wins (Pos (a' (X a)) b c d e f) X
--instance Wins (Pos a (X b) c d e f) X
--instance Wins (Pos a (b' (X b)) c d e f) X
-- need this for draws
-- instance (Valid a, b, c... etc) => Wins (Pos a b c d e f) Draw 

--whoWon :: Wins a b => a -> b
--whoWon a = undefined 
whoWon :: Done a b c d e f => (Pos a b c d e f,p) -> Bool 
whoWon a = True 

--(Wins (Pos as bs cs ds es fs)~False)) = 
move :: (Valid (a++as), Valid (b++bs), Valid (c++cs), Valid (d++ds), Valid (e++es), Valid (f++fs), p'<>p) => (Pos a b c d e f,p') -> (Pos as bs cs ds es fs,p) -> (Pos (a++as) (b++bs) (c++cs) (d++ds) (e++es) (f++fs),p')
move m b = undefined 

-- testing
vm :: (Valid a) => a -> Bool
vm _ = True
t1 = vm (undefined :: X++E)

b1 = move (pos1 X) (nb O) 
b2 = move (pos2 O) $ move (pos1 X) (nb O)
b3 = move (pos3 X) $ move (pos2 O) $ move (pos1 X) (nb O)
one2win = move (pos5 O) $ move (pos2 X) $ move (pos6 O) b1
wins = move (pos2 X) $ move (pos4 O) $ move (pos3 X) $ move (pos6 O) $ move (pos1 X) (nb O)

-- fails:
--mf = move pos1x pos1
--mf2 = move (pos2 X) $ move (pos1 X) (nb O) 

