{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- :set -fno-warn-missing-methods
import Data.Char

data State = XWins | YWins | Draw 

data Z
data S n

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three

zero :: Zero
zero = undefined

one :: One
one = undefined

two :: Two
two = undefined

three :: Three
three = undefined

four :: Four
four = undefined

-- without the functional dependency,  "f = t three" would be an ambigious type because ghc coudln't solve for b ... *even though there is only one instance of Three b*

class T a b | a->b where
  t::a->b

-- given anything, associate it with an Int
--instance T a Int where
--  t a = undefined

instance T Three State where
  t a = XWins 

instance T Two State where
  t a = YWins 

-- f :: State (with data XWins) 
f = t three 



class Convertible a b where
  convert :: a -> b

instance Convertible Int Char where
  convert = chr

instance Convertible Char Int where
  convert = ord

--without funct dep, the type must be specified
f':: Int 
f' = convert '*'



class Add a b c | a b -> c where
  add :: a -> b -> c

-- without UndecidableInstances turned on this will give an illegal declration
-- i.e Turning on UndecidableInstances loosens the constraint on context reduction that can only allow constraints of the class to become structural smaller than its head. 
{-- 
instance declaration for ‘Add (S a) b (S c)’
        The coverage condition fails in class ‘Add’
          for functional dependency: ‘a b -> c’
        Reason: lhs types ‘S a’, ‘b’
          do not jointly determine rhs type ‘S c’
        Un-determined variable: c
        Using UndecidableInstances might help
    • In the instance declaration for ‘Add (S a) b (S c)’
--}
instance Add Zero a a where
  add = undefined

instance Add a b c => Add (S a) b (S c) where
  add = undefined

-- unwrap one layer of type (i.e minus 1)
class Pred a b | a -> b where
  prev :: a -> b

instance Pred Zero Zero where
  prev = undefined

instance Pred (S n) n where
  prev = undefined

-- evaluate type to dat (i.e int)
class Eval a where
  eval :: a -> Int

-- wont care that it is an attempt to eval an undefined value, just cares about the type
instance Eval Zero where
  eval _ = 0

-- can run the eval function as long as it is on a type that conforms
-- i.e Zero, or any paeno number higher than zero
-- so Eval (S Z) works if Z is of type Eval (yes, it is)
-- so given anything (of a paeno type) add 1 and then eval its paeno number minus one
-- so Eval (S Z) ==> 1 + eval (Z)=> 1 + 0
instance Eval n => Eval (S n) where
  eval m = 1 + eval (prev m)







data Test = Test

instance Add Test a a where
  add = undefined

f'' :: c -> c
f'' = add Test 
