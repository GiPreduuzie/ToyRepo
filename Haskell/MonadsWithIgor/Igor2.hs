{-# LANGUAGE ParallelListComp #-}

ex = [x*y | x <- [1,2]| y <- [1,2,3]]

fibs = 1 : [a + b | a <- 0 : fibs | b <- fibs]

data Tree = EmptyTree | Node Int Tree Tree

showTree EmptyTree = "*"
showTree (Node x leftN rightN) = show x ++ "[" ++ showTree leftN ++ ", " ++ showTree rightN ++ "]"

--data Maybe' a = Just' a | Nothing'

safeHead [] = Nothing
safeHead (x:_) = Just x

n1 = 1