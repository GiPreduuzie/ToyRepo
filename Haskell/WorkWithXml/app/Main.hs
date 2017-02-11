module Main where

import System.IO
import System.Directory

import Lib


main :: IO () 
main =
   do 
    putStrLn "Current dir:"
    dir <- getCurrentDirectory
    putStrLn (show dir)
    putStrLn "------------"
    
    let surveyPath1 = "resources\\example_survey.xml"
    let surveyPath2 = "resources\\manually_constructed_survey.xml"
    
    handle <- openFile surveyPath2 ReadMode
    
    contents <- hGetContents handle
    
    putStr $ process contents
    putStrLn ""
    
    hClose handle