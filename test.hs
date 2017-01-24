{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}


import TicTacToe
import Board
import Constraints

class T a b c d e f g h i res | a b c d e f g h i -> res
instance T p p p d e f g h i p
instance T a b c p p p g h i p
instance T a b c d e f p p p p

-- XXX,OOO, not EEE
class S a b c r | a b c->r
--instance {-# OVERLAPPABLE #-} S a a c N
--instance {-# OVERLAPPABLE #-} S a b b N
--instance {-# OVERLAPPING #-} a<>E => S a a a a
--instance (S c b a E, S c a b E) => S a b c E

instance S X X X X
instance S O O O O
instance S E E E N 
instance S E E X N 


t :: S a b c r => (a,b,c)->r
t _ = undefined

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
instance W N N N N N N N w w


w :: W a b c d e f g h r => (a,b,c,d,e,f,g,h)->r
w _ = undefined

--w :: NoWins a b c d e f g h i => (a,b,c,d,e,f,g,h,i)->Bool
--w :: Wins a b c d e f g h i r => (a,b,c,d,e,f,g,h,i)->r
--w _ = undefined

