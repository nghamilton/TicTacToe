{-# LANGUAGE GADTs #-}

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

{--
data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test


eval :: Expr -> Maybe (Either Int Bool)

eval (I n) = Just $ Left n

eval (Add (B _)  _) = Nothing
eval (Add _ (B _)) = Nothing
eval (Add (Eq _ _)  _) = Nothing
eval (Add _ (Eq _ _)) = Nothing
eval (Add a b) = do -- maybe monad
  (Left a') <- eval a
  (Left b') <- eval b
  return $ Left (a'+b') --}
