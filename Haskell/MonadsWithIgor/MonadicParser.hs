{-# OPTIONS_GHC -W -fwarn-incomplete-uni-patterns #-}

newtype Parser a = Parser (a -> [(a, String)])

parser (Parser a) = a

parse (Parser f) value = f value

anyChar = Parser $ \ a -> case a of 
                          []     -> []
                          (x:xs) -> [([x], xs)]
						  
isChar c = Parser $ \ a -> case a of 
                          []     -> []
                          (x:xs) -> if x == c then [([x], xs)] else []
						  
						  
twiceAnyChar = Parser twice
               where twice a = case a of 
                                (x:y:xs) -> [([x,y], xs)]
                                _        -> []
							    
twiceAnyChar' = Parser $ \x -> concat [[(value ++ value', state') | (value', state') <- ((parser anyChar) state)] | (value, state) <- parser anyChar $ x]


anyChar2 = Parser func
                where func x = concat [ g value state | (value, state) <- parser anyChar $ x]
                      g value state = [(value ++ value', state') | (value', state') <- parser anyChar $ state]
                       
anyChar3 = Parser func
                where func x = concat [ g value state | (value, state) <- parser anyChar $ x]
                      g  value state = concat [g' (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g' value state = [(value ++ value', state') | (value', state') <- parser anyChar $ state]

anyChar4 = Parser func
                where func x = concat [ g value state | (value, state) <- parser anyChar $ x]
                      g   value state = concat [g'  (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g'  value state = concat [g'' (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g'' value state = [(value ++ value', state') | (value', state') <- parser anyChar $ state]
                      
anyChar5 = Parser func
                where func x = concat [ g value state | (value, state) <- parser anyChar $ x]
                      g    value state = concat [g'   (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g'   value state = concat [g''  (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g''  value state = concat [g''' (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      g''' value state = [(value ++ value', state') | (value', state') <- parser anyChar $ state]
                      
anyChar5' = Parser func
                where func x = g (g (g (g (g end)))) "" x
                      g h value state = concat [h (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      end value state = [(value, state)]
                      
anyChar5'' = Parser func
                where func x = (g . g . g . g . g $ end) "" x
                      g h value state = concat [h (value' ++ value) state' | (value', state') <- parser anyChar $ state]
                      end value state = [(value, state)]
                      
anyChar5''' = Parser func
                where func x = (g anyChar . g  (isChar 'b') . g anyChar . g anyChar . g anyChar $ end) "" x
                      g p h value state = concat [h (value' ++ value) state' | (value', state') <- parser p $ state]
                      end value state = [(value, state)]
                      


chain = Parser func
                where func x = (g anyChar >- isChar 'b' >- anyChar >- anyChar >- anyChar $ end) "" x
                      g p h value state = concat [h (value' ++ value) state' | (value', state') <- parser p $ state]
                      end value state = [(value, state)]
                      x >- y = x . g y
                      
chain' = 
    anyChar >- isChar 'b' >- anyChar >- anyChar >- anyChar
    where 
        x >- y = Parser (\state -> concat [ g y value' state' | (value', state') <- parser x $ state])
        g y value state = [(value ++ value', state') | (value', state') <- parser y $ state]
        
string "" = Parser $ \x -> [("", x)]
string (x:xs) = isChar x >- string xs
    where 
        x >- y = Parser (\state -> concat [ g y value' state' | (value', state') <- parser x $ state])
        g y value state = [(value ++ value', state') | (value', state') <- parser y $ state]
        
many  p = Parser $ \x -> (parser (many1 p) $ x) ++ [([], x)]
many1 p = p >- many p
    where 
        x >- y = Parser (\state -> concat [ g y value' state' | (value', state') <- parser x $ state])
        g y value state = [(value ++ value', state') | (value', state') <- parser y $ state]
