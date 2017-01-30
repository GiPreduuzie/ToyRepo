module IsHappy where


isHappy :: Integer -> Bool
isHappy x = isHappy' [] x


isHappy' raw x = 
    if (elem x raw)
    then False
    else 
        if (x == 1)
        then True 
        else isHappy' (x:raw) (sumSquares x)                      

sumSquares = sum . map (\x -> x*x) . toDigits

toDigits x | x <  0 = reverse $ toDigits' (-x)
           | x == 0 = [0]
           | x >  0 = reverse $ toDigits' x

toDigits' 0 = []
toDigits' x = mod x 10 : toDigits' (div x 10)
