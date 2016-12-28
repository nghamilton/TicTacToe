{-# LANGUAGE FlexibleContexts,AllowAmbiguousTypes,FlexibleInstances,RankNTypes,EmptyDataDecls #-}
 
data Z   = Z deriving (Show)
data S a = S a deriving (Show) 

class Board c where
  makeBoard ::(ValidBoard b)=>b->c
  makeBoard b = undefined 
 
instance Board Z
instance (Board c) => Board (S c)

class Board board => ValidBoard board 
{--where
  makeMove :: board -> Int -> board
  makeMove a p = case p of 
                   0 -> S a 
                   --1 -> S (S a)
--}

instance ValidBoard (S (S (S (S Z))))                  -- four
instance ValidBoard (S (S (S (S (S Z))))) -- five
instance ValidBoard (S (S (S (S (S (S Z)))))) -- six
instance ValidBoard (S (S (S (S (S (S (S Z))))))) -- seven
instance ValidBoard (S (S (S (S (S (S (S (S Z)))))))) -- eight

data Resistor board = Resistor deriving Show

d0  = undefined :: Z
d3  = undefined :: S (S (S Z))
d4  = undefined :: S (S (S (S Z)))
d6  = undefined :: S (S (S (S (S (S Z)))))
d8  = undefined :: S (S (S (S (S (S (S (S Z)))))))
d10 = undefined :: S (S (S (S (S (S (S (S (S (S Z)))))))))

-- accept only classes of board that are valid
-- but return a superclass type of Board (which could be valid or invalid)
-- i.e a d3 is both a ValidBoard and a Board instantiation: so we can return it as a Board, but then when supplying it to make a move, it will not conform
boardCheck:: (ValidBoard b) => b -> b 
boardCheck b = b 

--move:: S a -> S (S a)
move m b = boardCheck (makeMove m b)
