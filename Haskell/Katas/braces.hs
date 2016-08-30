module Codewars.Kata.Braces where

import Data.Maybe (fromMaybe)

validBraces :: String -> Bool
validBraces = check

check = fromMaybe False . fmap null . foldl fold (Just "")

pairs = [('(', ')'), ('[', ']'), ('{', '}')]          

fold accum current = accum >>= (applyBracket current)

applyBracket current accum = 
    if isOpening current 
    then return $ current:accum 
    else applyClosingBracket accum current
        
applyClosingBracket []     _ = Nothing
applyClosingBracket (y:ys) x = if isClosing y x then Just ys else Nothing

isOpening x   = elem x (map fst pairs)
isClosing l r = elem (l, r) pairs