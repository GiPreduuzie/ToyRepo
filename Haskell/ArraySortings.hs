s []     = []
s (x:[]) = [x]
s xs     =  mergeSorted (s left) (s right)
    where
        (left, right) = halves xs
        

halves xs = halves' xs [] []
    where 
        halves' []     a b =  (a, b)
        halves' (x:xs) a b =  halves' xs b (x:a)


mergeSorted  xs     []    = xs
mergeSorted  []     ys    = ys
mergeSorted (x:xs) (y:ys) = if x > y then x : mergeSorted  xs   (y:ys)
                                     else y : mergeSorted (x:xs) ys