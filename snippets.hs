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