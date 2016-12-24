import Control.Monad

data Board = Playable | Unplayable 

yes :: Board -> Maybe Board
yes = (\a->Just Playable)
no :: Board -> Maybe String 
no = (\a->Just "Unplayable")

buildMove :: Int -> Maybe Board 
buildMove m = Just Playable 

--move :: (Int, Board -> Maybe a) -> Int -> (a -> Maybe b) -> Maybe b
move :: (Int, Board -> Maybe a) -> a -> Maybe a 
move (m,b) = \b'-> case (m) of
  1 -> buildMove (m) >>= b >>= (\a -> return b')
  2 -> buildMove (m) >>= b >>= (\a -> return b')
--  3 -> Just "no" 
-- adding in this makes it a rigid type being returned (i.e a Just String)
-- need to know how to make a non-rigid type variable 
-- ... how to have return results undefined thoughout the program until the user first compiles?
newBoard = yes 

y = Just Playable >>=yes>>=yes>>=no
--n = Just Playable >>=yes>>=yes>>=no>>=yes
