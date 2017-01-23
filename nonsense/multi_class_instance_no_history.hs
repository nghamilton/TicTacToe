{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-} 

data X = X deriving (Show)
data O = O deriving (Show)
data E = E deriving (Show)
data Draw = Draw deriving (Show)

data Board a b c d e f = Board a b c d e f deriving (Show)
nb s = (Board E E E E E E)
pos1 p = (Board p E E E E E)
pos2 p = (Board E p E E E E)
pos3 p = (Board E E p E E E)
pos4 p = (Board E E E p E E)
pos5 p = (Board E E E E p E)
pos6 p = (Board E E E E E p)

class ValidMove a b c | a b -> c where
  update :: a->b->c
instance ValidMove E O O where
  update _ _ = O 
instance ValidMove O E O where  
  update _ _ = O 
instance ValidMove E X X where 
  update _ _ = X 
instance ValidMove X E X where
  update _ _ = X 
instance ValidMove E E E where 
  update _ _ = E 

move :: (ValidMove a a' a'', ValidMove b b' b'', ValidMove c c' c'', ValidMove d d' d'', ValidMove e e' e'', ValidMove f f' f'') => Board a b c d e f -> Board a' b' c' d' e' f' -> Board a'' b'' c'' d'' e'' f'' 
move m@(Board a b c d e f) board@(Board a' b' c' d' e' f') = Board (update a a') (update b b') (update c c') (update d d') (update e e') (update f f') 
