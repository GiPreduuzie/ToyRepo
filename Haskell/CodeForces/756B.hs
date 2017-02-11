import Control.Monad

goldCoins :: [Integer] -> [Integer]
goldCoins ts = ts

main = 
    do
        line1 <- getLine
        let n = read line1

        lines <- replicateM n getLine
        let ts = map read lines
        sequence . map (putStrLn . show) $ goldCoins ts
