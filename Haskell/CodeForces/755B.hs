import Data.List
import Control.Monad

runGame xs ys = 
    lx - ly + xbonus > 0
    where 
        common = length . takeWhile (\(x,y)-> x == y) $ zip (sort xs) (sort ys)
        lx = (length xs) - common
        ly = (length ys) - common
        xbonus = if common `mod` 2 == 0 then 0 else 1
        
getLines n = replicateM n getLine

main =
    do
        line1 <- getLine
        let numbers = map read . words $ line1
        let n = numbers !! 0
        let m = numbers !! 1
        selfWords  <- getLines n
        enemyWords <- getLines m
        
        putStrLn $
            if runGame selfWords enemyWords
            then "YES"
            else "NO"