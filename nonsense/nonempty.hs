infixr 5 :|
data NonEmpty a = a :| [a] deriving (Show)

--toList :: NonEmpty a -> [a]
--fromList :: [a] -> NonEmpty a
head :: NonEmpty a -> a
head (a :| _) = a

