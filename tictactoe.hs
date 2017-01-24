{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

import Data.Type.Equality
import GHC.Exts

{-- Todo:
* export only appropriate functions
* unit tests
* fix NoWin boilerplate
* implement remaining challenge questions
--}

data X = X deriving (Show) 
data O = O deriving (Show) 
data E = E deriving (Show)  
data Draw = Draw deriving (Show)  

class Result a
instance Result X
instance Result O
instance Result Draw 

infixr 5 +> 
data (+>) m ms = (:+>) m ms deriving (Show)  
type (<>) x y = (x == y) ~ False

type Win a b c = (a~b, b~c, a<>E)
type NoWins a b c d e f = (NoWin a b c, NoWin d e f) 
type ValidMerge a b c d e f as bs cs ds es fs = (Valid (a+>as), Valid (b+>bs), Valid (c+>cs), Valid (d+>ds), Valid (e+>es), Valid (f+>fs))
type ValidPlayer p p' = p'<>p 
type Finished a b c d e f = (Winner a b c d e f)

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
instance Empty e => Empty (E+>e)

-- an valid move - only empty moves before and after
class Valid a
instance Valid E
instance Empty e => Valid (X+>e)
instance Empty e => Valid (O+>e)
instance Valid v => Valid (E+>v)

-- Unwrap the list of moves 
class Unwrap a x | a -> x where
  unwrap :: a -> x
instance Unwrap E E where 
  unwrap _ = E 
instance Unwrap (X+>e) X where 
  unwrap _ = X
instance Unwrap (O+>e) O where 
  unwrap _ = O
instance (Unwrap a b) => Unwrap (E+>a) b where
  unwrap (a:+>as) = unwrap as 

class Winner a b c d e f
instance Winner X X X d e f
instance Winner a b c X X X 
instance Winner O O O d e f
instance Winner a b c O O O
instance {-# OVERLAPPABLE #-} (a<>E,b<>E,c<>E,d<>E,e<>E,f<>E) =>
  Finished a b c d e f

unwrapBoard
  :: (Unwrap a6 f, Unwrap a5 e, Unwrap a4 d, Unwrap a3 c,
      Unwrap a2 b, Unwrap a1 a) =>
     (Board a1 a2 a3 a4 a5 a6, t) -> Board a b c d e f
unwrapBoard ((Board a b c d e f),p) = Board (unwrap a) (unwrap b) (unwrap c) (unwrap d) (unwrap e) (unwrap f)

whoWon :: (Finished a' b' c' d' e' f', Unwrap a a', Unwrap b b', Unwrap c c', Unwrap d d', Unwrap e e', Unwrap f f') => (Board a b c d e f,p) -> Bool
whoWon x = whoWon' $ unwrapBoard x 

whoWon' :: Finished a b c d e f => Board a b c d e f -> Bool
whoWon' a = True

move :: (ValidMerge a b c d e f as bs cs ds es fs, ValidPlayer p p', NoWins a' b' c' d' e' f', Unwrap as a', Unwrap bs b', Unwrap cs c', Unwrap ds d', Unwrap es e', Unwrap fs f') => (Board a b c d e f,p') -> (Board as bs cs ds es fs,p) -> (Board (a+>as) (b+>bs) (c+>cs) (d+>ds) (e+>es) (f+>fs),p')
move move@(Board a b c d e f,p') board@(Board as bs cs ds es fs,p) = (board',p') where
 board' = Board (a:+>as) (b:+>bs) (c:+>cs) (d:+>ds) (e:+>es) (f:+>fs)
