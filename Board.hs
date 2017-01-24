{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-} 

module Board where

infixr 5 +>
data (+>) m ms = (:+>) m ms deriving (Show)

data X = X deriving (Show)
data O = O deriving (Show)
data E = E deriving (Show)

{--
data Draw = Draw deriving (Show)

class Result a
instance Result X
instance Result O
instance Result Draw
--}

data Result = XWon | OWon | Draw

data Board a b c d e f g h i = Board a b c d e f g h i deriving (Show)
new s = (Board E E E E E E E E E,s)
p1 p = (Board p E E E E E E E E,p)
p2 p = (Board E p E E E E E E E,p)
p3 p = (Board E E p E E E E E E,p)
p4 p = (Board E E E p E E E E E,p)
p5 p = (Board E E E E p E E E E,p)
p6 p = (Board E E E E E p E E E,p)
p7 p = (Board E E E E E E p E E,p)
p8 p = (Board E E E E E E E p E,p)
p9 p = (Board E E E E E E E E p,p)

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

