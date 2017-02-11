{-# OPTIONS_GHC -W -fwarn-incomplete-uni-patterns #-}

import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

-- (a -> b) -> Parser a -> Parser b

-- pure x = Parser $ \x -> [(x, x)]
-- Parser (a -> b) -> Parser b -> Parser b
-- apply f x = parser  

parse (Parser x) = x


is :: (Char -> Bool) -> (Char -> a) -> Parser a
is predicate f' = Parser f
                  where f x = case x of
                              (x:xs)  -> if predicate x then [(f' x, xs)] else []
                              []      -> []

--digit :: Parser Integer
digit = (is isDigit) (\x -> ord x - ord '0')

                           
into x = Parser $ \state -> [(x, state)]
addP p1 p2    = Parser $ \x -> ((parse $ p1) x ) ++  (parse $ p2) x
or' = addP
              
--many :: Parser a -> Parser [a]
many  p  = addP (many1 p) (into [])

--many1 :: Parser a -> Parser [a]
many1 p = bind (:) p (many p)

fmap' f p = Parser $ (\state -> [(f value', state') | (value', state') <- parse p $ state])
                           
number = fmap' (foldl (\accum x -> accum*10 + x) 0) (many digit)

isPlus :: Num a => Parser (a -> a -> a)
isPlus  = is (=='+') (\_ -> (+))
isMinus = is (=='-') (\_ -> (-))



operator =  
    ('+', (+)) `or` ('-', (-)) `or` ('*', (*)) `or` ('*', (/))
    where or (sign, function) = 
        
    [is (==sign) (\_ -> function) | (sign, function) <- operators]
    where operators = 
                     [('+', (+))
                     ,('-', (-))
                     ,('*', (*))
                     ,('*', (/))]




sum' = bind (\x y -> x y) (bind (\ x _ -> (x+)) number isPlus) number

y -< f = (fmap' (\x -> ($x)) y) >- f


f >- y = 
    Parser $ g f y
    where g f y state = concat [  [ (value' value'', state'') | (value'', state'') <- parse y $ state'] |(value', state') <- parse f $ state]

bind :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
bind g x y = Parser $ f x y
    where
        
        f x y state = concat $ f' (continue continue' y) x state
        
        f' continue x state = map continue (parse x $ state)
        
        continue continue' p (value, state)   = map (continue' value) (parse p $ state)
        
        continue' value (value', state') = (g value value', state')
        
        
        term = f * f * f * f .... f

        
