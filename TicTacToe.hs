{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module TicTacToe 
( whoWon 
, move
, X(X),O(O)
, p1,p2,p3,p4,p5,p6,p7,p8,p9,new
) where  

import Constraints 
import Board

{-- Todo:
* unit tests
* fix NoWin and p1..9 boilerplate
* implement remaining challenge questions
--}

unwrapBoard ((Board a b c d e f g h i),p) = Board (unwrap a) (unwrap b) (unwrap c) (unwrap d) (unwrap e) (unwrap f) (unwrap g) (unwrap h) (unwrap i)

whoWon :: (Finished a' b' c' d' e' f' g' h' i', Unwrap a a', Unwrap b b', Unwrap c c', Unwrap d d', Unwrap e e', Unwrap f f', Unwrap g g', Unwrap h h', Unwrap i i') => (Board a b c d e f g h i,p) -> Bool
whoWon x = whoWon' $ unwrapBoard x 

whoWon' :: Finished a b c d e f g h i => Board a b c d e f g h i -> Bool
whoWon' a = True

move :: (ValidMerge a b c d e f g h i as bs cs ds es fs gs hs is, ValidPlayer p p', NoWins a' b' c' d' e' f' g' h' i', Unwrap as a', Unwrap bs b', Unwrap cs c', Unwrap ds d', Unwrap es e', Unwrap fs f', Unwrap gs g', Unwrap hs h', Unwrap is i') => (Board a b c d e f g h i,p') -> (Board as bs cs ds es fs gs hs is,p) -> (Board (a+>as) (b+>bs) (c+>cs) (d+>ds) (e+>es) (f+>fs) (g+>gs) (h+>hs) (i+>is),p')
move move@(Board a b c d e f g h i,p') board@(Board as bs cs ds es fs gs hs is,p) = (board',p') where
 board' = Board (a:+>as) (b:+>bs) (c:+>cs) (d:+>ds) (e:+>es) (f:+>fs) (g:+>gs) (h:+>hs) (i:+>is)
