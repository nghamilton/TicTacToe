{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
import GHC.TypeLits
--import Data.Word
--import Foreign

data M (n :: Nat) = GetM 
data Board a b c = GetBoard deriving (Show)

start = GetBoard :: Board (M 0) (M 0) (M 0) 
m1 = GetBoard :: Board (M 1) (M 0) (M 0) 
m2 = GetBoard :: Board (M 0) (M 1) (M 0) 
m3 = GetBoard :: Board (M 0) (M 0) (M 1) 

--type M1 = Board (M 1) (M 0) (M 0) 

addBoard :: Board (M a) (M b) (M c) -> Board (M a') (M b') (M c') -> Board (M (a+a'))(M (b+b'))(M (c+c'))
addBoard _ _ = GetBoard

validBoard :: (a<=1,b<=1,c<=1)=> Board (M a) (M b) (M c) -> Board (M a) (M b) (M c)
validBoard = id

whoWon :: (3<=(a+b+c))=> Board (M a) (M b) (M c) -> Bool 
whoWon _ = True

move m b = addBoard m b
