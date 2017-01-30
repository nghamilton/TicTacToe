{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TicTacToe 
( whoWon 
, move
, X(X),O(O)
, p1, p2, p3, p4, p5, p6, p7, p8, p9,new
) where  

import Constraints 
import Board

{-- Todo:
* unit tests
* scrap Winner by testing for draw or win - resolve conflicting deps probelem?
* fix Res and p1..9 boilerplate nonsense
* fix Unwrap 
* implement remaining challenge questions
--}

move :: (IsValidMerge a b c d e f g h i as bs cs ds es fs gs hs is, IsValidPlayer p p', HasNoWins a' b' c' d' e' f' g' h' i', Unwrap as a', Unwrap bs b', Unwrap cs c', Unwrap ds d', Unwrap es e', Unwrap fs f', Unwrap gs g', Unwrap hs h', Unwrap is i') => (Board a b c d e f g h i, p') -> (Board as bs cs ds es fs gs hs is, p) -> (Board (a+>as) (b+>bs) (c+>cs) (d+>ds) (e+>es) (f+>fs) (g+>gs) (h+>hs) (i+>is), p')
move move@(Board a b c d e f g h i, p') board@(Board as bs cs ds es fs gs hs is, p) = (board', p') where
 board' = Board (a:+>as) (b:+>bs) (c:+>cs) (d:+>ds) (e:+>es) (f:+>fs) (g:+>gs) (h:+>hs) (i:+>is)

unwrapBoard :: (Unwrap a a', Unwrap b b', Unwrap c c', Unwrap d d', Unwrap e e', Unwrap f f', Unwrap g g', Unwrap h h', Unwrap i i') => (Board a b c d e f g h i, p) -> Board a' b' c' d' e' f' g' h' i' 
unwrapBoard ((Board a b c d e f g h i), p) = Board (unwrap a) (unwrap b) (unwrap c) (unwrap d) (unwrap e) (unwrap f) (unwrap g) (unwrap h) (unwrap i)

whoWon :: (Unwrap as a, Unwrap bs b, Unwrap cs c, Unwrap ds d, Unwrap es e, Unwrap fs f, Unwrap gs g, Unwrap hs h, Unwrap is i, HasWins a b c d e f g h i, Result win) => (Board as bs cs ds es fs gs hs is, p) -> win 
whoWon x = get 
