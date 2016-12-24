{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

-- higher-rank type allows for 'c' - i.e without it there would be a type variable not in scope error for 'c' 
newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b
first p = runPair p (\x y -> x) 
second p = runPair p (\x y -> y) 

class Board b where
  t :: b 
instance Board GoodBoard where
  t = undefined 
instance Board BadBoard where
  t = undefined 

data GoodBoard = G Int
data BadBoard = B Char 

--make f only return types that are instances of Board
--i.e. same as:
--f :: Board a => Int -> a
-- ... but BadBoard not accepted as type of Board
f :: Int -> (forall a.Board a=>a)
f i = case i of 
  1 -> G 1 
  2 -> B 'a'
