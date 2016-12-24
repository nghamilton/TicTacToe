{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, RankNTypes #-}
{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures, TypeOperators #-}

import Data.Bits
class OK_ b
class Board_ b where
  extract :: b
--  fail :: NO 

instance Board_ OK where
  extract = K 
 -- fail = N

instance Board_ NO where
  extract = N 
  --fail = N

instance OK_ OK

data OK = K deriving (Show)
data NO = N deriving (Show)

m :: Bool -> (Board_ a=>a)
-- ?? fails because it unifies a?
--m b = K
-- this works as it doesn't unify a:
m b = extract 

-- m True can be used in either of these two functions below
o :: OK -> Bool
o b = True

n :: NO -> Bool
n b = False

t :: OK -> OK 
t b = b 
f :: NO -> NO 
f b = b 
--- but still left with the problem how to return a function of different types

{--data Board :: *->* where
  KKK :: Board OK
  NNN :: Board NO 

move :: Bool -> Board 
move p = case p of 
  True -> KKK 
  False -> NNN 

--ext :: forall b. Board_ b => Board -> b
--ext b = extract b 

run :: Board OK -> Bool
run b = true
--}

{-- data Baz = forall a. Eq a => Baz1 a a
         | forall b. Show b => Baz2 b (b -> b)

f :: Baz -> String
f (Baz1 p q) | p == q    = "Yes"
             | otherwise = "No"
--f (Baz1 v fn)            = show (fn v)
-- above fails as it is expecting something that returns a instance of show
f (Baz2 v fn)            = show (fn v)

a = Baz1 1 1
b = Baz2 1 id

--}
