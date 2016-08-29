module Main where

import System.IO

import Lib


main :: IO () 
main =
   do 
    handle <- openFile "..\resources\example_survey.xml" ReadMode
    
    contents <- hGetContents handle
    
    putStr $ process contents
    putStrLn ""
    
    hClose handle