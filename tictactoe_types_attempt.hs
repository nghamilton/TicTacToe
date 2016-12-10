type Player = Char
type Position = Integer

data Move = Play (Player,Position)
  deriving (Show, Eq)

{--data Move = Unplayed
          | Play (Player,Position)
  deriving (Show, Eq)
          
data Board = Empty 
           | Moves Move Board
  deriving (Show, Eq)
--}

--class TicTacToe x where
--  move :: x -> x 

-- problem with this is that the typeclass implementation will require all but one of the function parameters to be supplied before it will work
-- ... therefore need to have a method that takes a move and returns a move if wnat to use typeclass methods?
--board :: Move -> Move -> Move -> Move -> Move -> Move -> Move -> Move -> Move -> String 
--board _ _ _ _ Unplayed Unplayed Unplayed Unplayed Unplayed = "Not Done" 
--board _ _ _ _ _ _ _ _ _ = "Done" 


-- a board can be either 
--  1) a function that takes a move and returns a board
--  2) a finished game 


--type Play = Int -> Board

data Board = Unplayable 
           | Playable [Int]

move :: (Board -> Int) -> Board
move f = 




{--

lastMove = (\m->Unplayable)
validMoveX = (\m->lastMove)

validMoveX = (\m->case m of 
              (Play (_,8))->lastMove
              (Play (_,_))->validMoveX)
              --(Play ('Y',_))->validMoveY m)
validMoveY = (\m->case m of 
              (Play (_,8)) ->lastMove
              (Play ('X',_))->validMoveX m)
{--}


newBoard = validMoveX
--------
board0 = newBoard 
board1 = board0 (Play ('X',1))
board2 = board1 (Play ('X',7))
-- wont compile
--board3 = board2 (Play ('X',2))

--- PROBLEM: need to make Playable also of type Move -> Move -> Board

--}


