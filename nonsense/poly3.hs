{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
import Control.Monad.ST

type Play = Int
class OKBoardType a where 
  getMoves :: a->[Int]
class BoardType a where 
  getMoves2 :: a->[Int]
class BoardClass a where
  doWithType :: a -> [Int] --(forall b.BoardType b=>b)
   
data Board = forall b.BoardType b => B b | N 

data OK = OK [Int] deriving (Show)
data NO = NO deriving (Show) 

instance OKBoardType OK where
  getMoves (OK ms)  = ms 
instance BoardType OK where
  getMoves2 (OK ms)  = ms 
instance BoardType NO where
  getMoves2 _  = [] 


instance BoardClass Board where
  doWithType (B s) = getMoves2 s 

list :: [Board]
list = [B NO, B (OK [1,2,3])]

-- need something like this:
-- instance OKBoardType (Board OK) where

-- ? ** need to understand the differences
--run :: Int -> (forall b.OKBoardType b =>b) -> Board
-- ? why this work

-- or something like this:
--run :: (Board OK) -> Int -> Board
run :: Board -> Int -> Board
run (B m) p = case p of  
  1 -> B $ OK $ [p]++getMoves2 m 
  2 -> N 

newBoard = B $ OK []

{--
compiles and works.. 
class BoardClass a where 
  t :: a

instance Show Board where
  show (Board s) = show s

--data Board = forall s.BoardClass s => Board s
data Board = forall s.Show s => Board s

data OK = OK [Integer] deriving (Show)
data NO = NO deriving (Show) 
instance BoardClass OK where
  t = OK []

list :: [Board]
list = [Board "2", Board True]

--}

--move :: Int -> Int -> Board
--move p b = case p of  
--  1 -> Board t 
--  2 -> Board NO

--m' :: Int -> Board OK -> (forall b. Board b) 
--m' p b = case p of 
--  1 -> Board OK 
--  2 -> Board NO 

-- With different type constructors 
-- take a play, and a valid board, and return a valid or invalid board type 

-- With a custom data type
-- take a play and a valid board (custom data type, list of valid plays) and return a valid or invalid board (list of valid plays + invalid play)

--  ST monad?
--v   = runST (newSTRef "abc")
--foo = runST (readSTRef v)

--mkZero :: forall s. ST s (STRef s Int)
--mkZero = newSTRef 0


--move :: Play -> OkBoard -> (forall b.a->String)
--move p b = undefined
--case p of 
--  1 -> (\a -> show a) -- OK
  --2 -> N

-- With different type constructors 
-- take a play, and a valid board, and return a valid or invalid board type 

-- With a custom data type
-- take a play and a valid board (custom data type, list of valid plays) and return a valid or invalid board (list of valid plays + invalid play)

--  ST monad?
--v   = runST (newSTRef "abc")
--foo = runST (readSTRef v)

--mkZero :: forall s. ST s (STRef s Int)
--mkZero = newSTRef 0

