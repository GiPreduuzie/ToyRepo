module Main where

import Lib
import Data.Conduit
--import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import Control.Monad

main :: IO ()
main = do putStrLn $ show ( source $$ conduit' =$ sink')
