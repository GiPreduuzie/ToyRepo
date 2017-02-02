goldCoins xs = sum $ [m - x | x <- xs ] where m = maximum xs 

main = 
    do
        line1 <- getLine
        line2 <- getLine
        let coins = map read . words $ line2
        putStrLn . show $ goldCoins coins
