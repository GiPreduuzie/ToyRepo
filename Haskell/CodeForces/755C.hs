cluster::[(Integer, Integer)] -> [[Integer]]
cluster pairs = foldr addToCluster [] pairs

addToCluster (x, y) accum =
    if elem y xArray
    then xArray : accum'
    else (xArray ++ yArray) : accum''
    where (accum' , xArray) = getArrayFor accum  x
          (accum'', yArray) = getArrayFor accum' y

getArrayFor accum x = if null result
                      then (accum, [x])
                      else (filter (\ y -> not $ elem x y ) accum , head result)
                      where result = filter (\ y -> elem x y ) accum

main = 
    do
        _     <- getLine
        line2 <- getLine
        putStrLn . show . length . cluster . zip [1..] . map read . words $ line2
