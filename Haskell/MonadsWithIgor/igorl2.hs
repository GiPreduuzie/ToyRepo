import Control.Applicative
import Control.Monad

--data Writer a = Writer a String deriving Show
data Writer a = Writer a String deriving Show

--map' f (Writer a s) = Writer (f a) s
instance Functor Writer where
    fmap f (Writer a s) = Writer (f a) s

instance Applicative Writer where
    pure a = Writer a ""
    (<*>) (Writer f s_1) (Writer a s_2) = Writer (f a) (s_1++s_2)
    
instance Monad Writer where
    return a = Writer a ""
    (>>=) (Writer a s) f = Writer a' (s++s') where (Writer a' s') = f a

toS (Writer a s) = Writer (show a) s

--add :: Num a => Writer a -> Writer a -> Writer a
--add (Writer a s_a) (Writer  b s_b) = Writer (a+b) (s_a++s_b)
--addOne (Writer a s) = Writer (a+1) s

--x f (Writer a s_a) (Writer  b s_b) = Writer (f a b) (s_a++s_b)

--log' :: String -> Writer ()
log' str = Writer () str
addW a b = Writer (a+b) "empty"

t = addW <$> (Writer 1 "1") <*> (Writer 2 "2")
t1 a b = do 
        s1 <- a
        s2 <- b
        log' "sdfsdf"
        return $ s1+s2

t2 = (Writer 1 "1") >>= \s1 -> (Writer 2 "2") >>= \s2 -> log' "sdf" >>= \_ -> addW s1 s2


------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

data State a b = State (a -> (a, b))

instance Functor (State n) where
    fmap f (State g) = State function where
                                        function t = (a1, (f b1)) where (a1, b1) = (g t)

instance Applicative (State n) where
    pure a = State (\x -> (x, a))
    (<*>) (State g1) (State g2) = State function where
                                        function t = (a2_1, (b1_2 b2_2)) where (a1_1, b1_2) = (g1 t)
                                                                               (a2_1, b2_2) = (g2 a1_1)

instance Monad (State n) where
    return a = State (\x -> (x,a))
    (>>=) (State g) f = State function where
                              function t = (x, y) where (a, b) = (g t)
                                                        State r = f b
                                                        (x, y) = r a

                                                        
f x y = do
          x' <- x
          y' <- y
