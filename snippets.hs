import Data.Array

knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
---------------------------

embedded list comprehension:
[ [ x | x <- xs, even x ] | xs <- xxs]  
---------------------------

-- file: ch05/Prettify.hs
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col



---------------------------


compress :: Eq a => [a] -> [a]
compress [] = []
compress (z:zs) = z : foldr f (const []) zs z
  where f x k w | x==w      = k x
                | otherwise = x : k x


-----------------------------------
-- take the head, put all elements that are smaller on the left, and all elments that are larger on the right..
--- and sort the left and right
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  



    getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  



----------------------------------------------------------------
['x','y'] >>= \m -> [1..9] >>= \p -> return (m,p)
[('x',1),('x',2),('x',3),('x',4),('x',5),('x',6),('x',7),('x',8),('x',9),('y',1),('y',2),('y',3),('y',4),('y',5),('y',6),('y',7),('y',8),('y',9)]

--or 
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  


------
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  


in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  

--or

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  

--- Monadic Writer
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)  



--Creates a superset (i.e all possible sets of 1..9)
filterM (\x -> [True, False]) [1..9] 




-- traceFamily is a generic function to find an ancestor
traceFamily :: Sheep -> [ (Sheep -> Maybe Sheep) ] -> Maybe Sheep
traceFamily s l = foldM getParent s l
  where getParent s f = f s
 
-- we can define complex queries using traceFamily in an easy, clear way
mothersPaternalGrandfather s = traceFamily s [mother, father, father]
paternalGrandmother s = traceFamily s [father, mother]



-- a Dict is just a finite map from strings to strings
type Dict = FiniteMap String String
 
-- this an auxilliary function used with foldl
addEntry :: Dict -> Entry -> Dict
addEntry d e = addToFM d (key e) (value e)
 
-- this is an auxiliiary function used with foldM inside the IO monad
addDataFromFile :: Dict -> Handle -> IO Dict
addDataFromFile dict hdl = do contents <- hGetContents hdl
                              entries  <- return (map read (lines contents))
                              return (foldl (addEntry) dict entries)
 
-- this program builds a dictionary from the entries in all files named on the
-- command line and then prints it out as an association list
main :: IO ()
main = do files   <- getArgs
          handles <- mapM openForReading files
          dict    <- foldM addDataFromFile emptyFM handles
          print (fmToList dict)


-- lookup table example to run a command
dispatch =  [ ("add", add)              , ("view", view)              , ("remove", remove)              ] 
let (Just action) = lookup command dispatch


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x  



--- call the correct function based on return type required
class MyClass a b where
  fn :: (a,b)
instance MyClass Int Int where
  fn = (1,1)

example :: (Int, Int)
example = fn


--------- GHCI
-- show types automatically (of evaluated expressions)
:set +t 



-- Use functional dependencies to add
-- the definition of Add suggests that given an 'a' and a 'b', return a 'c' 
-- .. the funct dep. allows the type checker to resolve what c is, when given an 'a b' unique pair
class Add a b c | a b -> c where
  add :: a -> b -> c
instance Add Zero a a where
  add = undefined
-- so given an Zero type and an a type, we know the add function will always now return an 'a' type
-- .. this is because Zero and 'a' supplied to add will produce a 'c' -- in this case the 'c' is the third argument in the instance -- i.e the 'a' type variable
--.. so this means that given a Zero, and any other type, the other type will be the result of the add function
instance Add a b c => Add (S a) b (S c) where
  add = undefined
-- for the case of adding one (i.e add::(S Z)->b-> ..) to any number: 
-- the type checker checks if there is a instance of the typeclass for (S a) b (S c), which requires a check if there is an instance of Add a b c ... which checks for Add Z b c .. this is true for all cases where b~c .. therefore the result is (S b) i.e add one more to b
-- for the case of adding two and one (i.e add (S (S Z))->(S Z)-> c ) ==> Add (S (S Z)) (S Z) (S c1) (i.e a b c) ... needs a check if (S Z) (S Z) c1 is an instance ==> which needs a check if Z (S Z) c2 is an instance, which it is when c2~(S Z), therefore c1 resolves to be (S c2) which is (S (S Z), and therefore c resolves from (S c1) to (S (S (S Z))
