{-# LANGUAGE ParallelListComp #-}

x = 2


square f x =  f x * f x

add a b = a+b

data Maybe' a = Just' a |Nothing'

printMaybe (Just x) = "Just: " ++ show x
printMaybe Nothing  = "Nothing"

factorial n
           | n <  1    = Nothing
           | n == 1    = Just 1
           | otherwise =  do  factorialN <- factorial (n - 1)
                              return $ factorialN * n
           
factorial' n1 Nothing   = Nothing
factorial' n1 (Just n2) = Just $ n1 * n2




y = [1..]
z = [a + b | a <- [1..] | b <- [1,2..] ]
fibs = 1 : [a + b | a <- 0 : fibs | b <- fibs]



-- import qualified Data.Map as Map

-- addingFunction x y = x + y

-- fibs = 1 : [a + b | a <- 0 : fibs | b <- fibs]

-- list = [Nothing, Just 1, Nothing, Just 2]


-- toStringList list = map toString list
                    -- where toString Nothing  = "Nothing"
                          -- toString (Just x) = show x
                          
-- dictionary = Map.fromList [("a", "something"), ("c", "something else")]


-- map (\ x -> Map.lookup x dictionary) ["a", "b", "c", "d"]

-- --getParent x 

-- add1 maybe1 maybe2 = 
    -- do
       -- value1 <- maybe1
       -- value2 <- maybe2
       -- return (value1 + value2)
       

    
                          
-- -- function maybe accum =
   -- -- do
      -- -- value <- maybe
      -- -- accum ++ ", " ++ value
      -- -- return value