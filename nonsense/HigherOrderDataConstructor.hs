data Funny f = Funny Integer f

f :: Funny Int-> String
f x = "ok"
s = Funny 1
runTest :: (t, t -> Funny Int) -> String
runTest (m,s) = f (s m)
runTestT = f (s 1)
-- won't compile:
-- runTestT = runTest('1',s)

newtype Playable = Playable Int
newtype Unplayable = Unplayable Int
data Board t = Board [Int] t Int
  deriving (Show)

--move' :: Int -> Board Playable -> String
--move' m (Board l mvs) = show (m:l:mvs)

newBoard = Board [] Playable
--move :: (Int, m->Board Playable) -> String -- (m->Board Playable) 
move (m,t) = let moves = checkMove (t m) in
             if (not $ null moves)
               then Board moves Playable
               else undefined --Board moves Unplayable
--errors as cant have type returning both ....
--move :: (t, t -> Board Playable) -> Int -> Board (Int -> Playable)
--and not:
--move :: (t, t -> Board Playable) -> Int -> Board (Int -> UnPlayable)

checkMove :: Board Playable -> [Int]
checkMove (Board ms s m) = m:ms --"Move okay, current state: " ++ show (m:ms)
