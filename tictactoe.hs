type Moveer = Char
type Position = Integer
type Move = (Moveer,Position)
data Board = Empty 
           | Moves Move Board
  deriving (Show, Eq)

