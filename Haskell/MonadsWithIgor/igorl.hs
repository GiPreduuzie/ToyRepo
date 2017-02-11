import Control.Monad

add a b = a+b
mul a b = a*b

sel [] f = []
sel (a:xs) f = f a : sel xs f

comp  f1 f2 x = f1 $ f2 x
comp' f1 f2 = \x -> f1 $ f2 x

data Maybe' a = Nothing' | Just' a deriving (Show)

inc Nothing' = Nothing'
inc (Just' x) = Just' $ add 1 x

f' x = fmap (add 1) x

findIn [] key = Nothing
findIn ((a,b):xs) key = if a==key then Just b else findIn xs key

tree = [(1,2),(2,3),(3,4)]

findGranPa man = case t of
                        Nothing -> Nothing
                        (Just x) -> findIn tree x
                 where t = (findIn tree man)

findGran2Pa man = case t of
                        Nothing -> Nothing
                        (Just x) -> findIn tree x
                 where t = (findGranPa man)
                 
findGran2Pa' man = Just man >>= findIn tree >>= findIn tree >>= findIn tree


-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

--allmul = [a*b | a<-[100..999], b<-[100..999] ]
--allpol [a] = [ x | x<-a, isPol ]

allmull = map (\(a,b) -> a*b) $ concat $ map (\x-> map (\y-> (x,y)) [100..999]) [100..999]
ispol x = show x == reverse (show x)
allpol = filter ispol allmull
max_ = maximum allpol

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

t::[a] -> [a]
t x = take 10 x


imum value = 
       do x <- value
          y <- value
          let m = x*y
          guard' (ispol m)
          [m]

m = maximum $ imum [100..999]  
o x = imum x     

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

guard' o = if o then "o" else []

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

data Writer a = Writer a String

add :: a -> a -> a
add a b = a+b
























