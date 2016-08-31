{-# OPTIONS_GHC -W -fwarn-incomplete-uni-patterns #-}

import Data.Char

newtype Parser a = Parser (a -> [(a, String)])

parser (Parser x) = x

anyChar = Parser f
          where f x = case x of
                       (x:xs) -> [([x], xs)]
                       []     -> []         
                       
isChar c = is (== c)
                       
is predicate = Parser f
               where f x = case x of
                           (x:xs)  -> if predicate x then [([x], xs)] else []
                           []      -> []
                       
anyChar2 = Parser f
 where
   f state = concat [ g value' state' | (value', state') <- parser anyChar $ state]
   g value state = [ (value ++ value'', state'') |(value'', state'') <- parser anyChar $ state]

anyChar3 = Parser f
 where
   f state = concat [ g value' state' | (value', state') <- parser anyChar $ state]
   g  value state = concat [ g' (value ++ value'') state'' | (value'', state'') <- parser anyChar $ state]
   g' value state = [ (value ++ value'', state'') | (value'', state'') <- parser anyChar $ state]

anyChar4 = Parser f
 where
   f state = concat [ (g (g g'')) value' state' | (value', state') <- parser anyChar $ state]
   g  h value state = concat [ h (value ++ value'') state'' | (value'', state'') <- parser anyChar $ state]
   g'' value state = [ (value ++ value'', state'') | (value'', state'') <- parser anyChar $ state]

-- anyChar5 = g (isChar '1') >- anyChar2 >- anyChar >- anyChar >- isChar '2'
 -- where
   -- --f state = (g (isChar '1') >- anyChar2 >- anyChar >- anyChar >- isChar '2' $ end) "" state
   -- g p h value state = concat [ h (value ++ value') state' | (value', state') <- parser p $ state]
   -- end value state = [(value, state)]
   -- x >- y = Parser $ \n -> (x . g y) "" n
   
x >- y = Parser $ f x y
    where
        --f x y = (\state -> concat $ map (\ (value', state') -> map (\ (value'', state'') -> (value' ++ value'', state))  parser y $ state )  parser x $ state)
        f x y state = concat $ f' (continue continue' y) x state
        
        f' continue x state = map continue (parser x $ state)
        
        continue continue' p (value, state)   = map (continue' value) (parser p $ state)
        
        continue' value (value', state') = (value ++ value', state')
        
        -- f  x y state       = f' (g end y) x "" state
        -- f' continue p value state           = concat $ [continue value value' state' |(value', state') <- parser p $ state]
        -- g  continue p value value'' state'' = concat $ [continue value value' state' |(value', state') <- parser p $ state'']
        
        -- end value value' state' = [(value ++ value', state')]
        
        --h continue p value state = 
              
        
        
        --1 f x y state        = concat $ h (\value' state' -> g  y   value'         state') x state
        --2 f x y state        = concat $ h (g  y) x state
        -- g y value state    = concat $ h (\value' state' -> end (value ++ value') state') y state
        -- end value state    = [(value, state)]
        -- h continue p state = [continue value' state' |(value', state') <- parser p $ state]

into value  = Parser $ \x -> [(value, x)]
addP p1 p2    = Parser $ \x -> ((parser $ p1) x ) ++  (parser $ p2) x
 
chain = 
    anyChar >- isChar 'b' >- anyChar >- anyChar >- anyChar
        
string ""     = into ""
string (x:xs) = isChar x >- string xs

many  p  = addP (many1 p) (into "")
many1 p = p >- many p 


sepby  p sep = addP (sepby1 p sep) (into "")
sepby1 p sep = p >- many( sep >- p )

digit = many (is isDigit)
