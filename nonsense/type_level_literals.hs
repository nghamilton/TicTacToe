{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.TypeLits
import Data.Word
import Foreign

-- ? uses KindSignatures to make l have kind of Symbol
data Label (l :: Symbol) = Get
data Numb (n :: Nat) = GetNumb

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point = Point Int Int deriving Show

instance Has Point "x" Int where from (Point x _) _ = x
instance Has Point "y" Int where from (Point _ y) _ = y

-- DataKinds ensures support for Label "x"
example = from (Point 1 2) (Get :: Label "x")

e2 = (GetNumb :: Numb 2)
e3 = (GetNumb :: Numb (1+2))

-- t' (GetNumb :: Numb 1) works
-- but t' (GetNumb :: Numb 2) fails type checking
t' :: (a<=15) => Numb a -> Int
t' a = 1

add :: Numb a -> Numb b -> Numb (a+b) 
add _ _ = GetNumb 

x16 = (GetNumb :: Numb 16)
x10 = (GetNumb :: Numb 10)
v = natVal $ add x16 x10


--t :: x<=?10 => Numb x -> Bool
--t a = True
