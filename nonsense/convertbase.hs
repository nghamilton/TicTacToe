nums = [1..20]::[Int]
base = 3::Int

convert :: [Int]->Int->[Int]
convert [] _ = []
convert ns b = convert' (ns,0,0) b

convert' :: ([Int],Int,Int)->Int->[Int]
convert' ([],_,_) _ = []
convert' (n:ns,0,0) b = convert' (ns,c',r') b where
  (c',r') = n `divMod` b 

convert' (n,c,r) b
  | c == 0 = r:(convert' (n,0,0) b)
  | c > 0 && r == 0 = c:convert' (n,0,0) b
  | otherwise = convert' (n,c',r*10+r') b where
    (c',r') = c `divMod` b
    (mult,multMod) = c `divMod` b
