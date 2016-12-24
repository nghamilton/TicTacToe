{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
import Data.Bits
data OK = OK [Int] deriving (Show)
data NO = NO deriving (Show)

move :: Bool -> (forall b. Int->b)
move p = case p of 
  True -> ((!!)[1,2,3])
  False -> bit 

