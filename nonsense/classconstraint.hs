{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
data Safe = Safe
data NotSafe
class SafeClass a where
  f :: a
instance SafeClass Safe where
  f = Safe

data Board y s = BY y s | BN NotSafe s

m :: SafeClass a=> b -> Int ->a
m b p = case p of 
  1 ->Safe 
  --2 ->BN
