import Data.Array
import Control.Monad

type Magic = Int
data Player = Empty | X | O
  deriving (Show,Eq)
type Move = Int
type MovePlayer = (Move,Player)

type Pos = (Int,Int)
type Play = (Magic,Player)
type BState = Array Pos Play
type PosState = (Pos,Play)
data BoardState = Unplayable
           | Playable {state :: BState}
           | XWon {state :: BState}
           | OWon {state :: BState}
           | Drawed {state :: BState}
  deriving (Show,Eq)

newtype Board a = Board { move' :: Int -> Board a}

type NBoard = Board Player

maxsize = 3
empty = (0,Empty)
validMoves = [1..maxsize]
--}
{-- 
instance Monad Board where
  return p = newBoard 
  b >>= f = undefined 
}
