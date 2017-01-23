{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}

import Data.Type.Equality
import GHC.Exts
import Prelude hiding ((++))

data X = X deriving (Show) 
data O = O deriving (Show) 
data E = E deriving (Show)  
data Draw = Draw deriving (Show)  

class Result a
instance Result X
instance Result O
instance Result Draw 

infixr 5 ++ 
data (++) m ms = M m ms deriving (Show)  
type (<>) x y = (x == y) ~ False

class NoWin a b c
instance NoWin X O X 
instance NoWin O X X 
instance NoWin X X O 
instance NoWin O X O 
instance NoWin O O X 
instance NoWin X O O 
instance NoWin E b c 
instance NoWin a E c 
instance NoWin a b E 

type Wins a b c = (a~b, b~c, a<>E)
type NoWins a b c d e f = (NoWin a b c, NoWin d e f) 

data Board a b c d e f = Board a b c d e f deriving (Show)
nb s = (Board E E E E E E,s)
pos1 p = (Board p E E E E E,p)
pos2 p = (Board E p E E E E,p)
pos3 p = (Board E E p E E E,p)
pos4 p = (Board E E E p E E,p)
pos5 p = (Board E E E E p E,p)
pos6 p = (Board E E E E E p,p)  

-- an empty move 
class Empty a
instance Empty E
instance Empty e => Empty (E++e)

-- an valid move - only empty moves before and after
class Valid a
instance Valid E
instance Empty e => Valid (X++e)
instance Empty e => Valid (O++e)
instance Valid v => Valid (E++v)

-- Unwrap the list of moves 
class Unwrap a x | a -> x where
  unwrap :: a -> x
instance Unwrap E E where 
  unwrap _ = E 
instance Unwrap (X++e) X where 
  unwrap _ = X
instance Unwrap (O++e) O where 
  unwrap _ = O
instance (Unwrap a b) => Unwrap (E++a) b where
  unwrap (M a as) = unwrap as 

class Finished a b c d e f
instance Finished X X X d e f
instance Finished a b c X X X 
instance Finished O O O d e f
instance Finished a b c O O O
instance (a<>E,b<>E,c<>E,d<>E,e<>E,f<>E)=>Finished a b c d e f

unwrapBoard ((Board a b c d e f),p) = Board (unwrap a) (unwrap b) (unwrap c) (unwrap d) (unwrap e) (unwrap f)

--whoWon :: ((Finished a' b' c' d' e' f'), Unwrap a a', Unwrap b b', Unwrap c c', Unwrap d d', Unwrap e e', Unwrap f f') => (Board a b c d e f,p) -> Bool

whoWon x = whoWon' $ unwrapBoard x 
whoWon' :: Finished a b c d e f => Board a b c d e f -> Bool
whoWon' a = True

move :: (Valid (a++as), Valid (b++bs), Valid (c++cs), Valid (d++ds), Valid (e++es), Valid (f++fs), p'<>p) => (Board a b c d e f,p') -> (Board as bs cs ds es fs,p) -> (Board (a++as) (b++bs) (c++cs) (d++ds) (e++es) (f++fs),p')
move move@(Board a b c d e f,p') board@(Board as bs cs ds es fs,p) = (Board (M a as) (M b bs) (M c cs) (M d ds) (M e es) (M f fs),p')





-- testing
vm :: (Valid a) => a -> Bool
vm _ = True
t1 = vm (undefined :: X++E)

b1 = move (pos1 X) (nb O) 
b2 = move (pos2 O) $ move (pos1 X) (nb O)
b3 = move (pos3 X) $ move (pos2 O) $ move (pos1 X) (nb O)
one2win = move (pos5 O) $ move (pos2 X) $ move (pos6 O) b1
draw = move (pos3 O) $ move (pos4 X) $ move (pos5 O) $ move (pos2 X) $ move (pos6 O) b1
wins = move (pos2 X) $ move (pos4 O) $ move (pos3 X) $ move (pos6 O) $ move (pos1 X) (nb O)

-- fails:
--mf = move pos1x pos1
--mf2 = move (pos2 X) $ move (pos1 X) (nb O) 

