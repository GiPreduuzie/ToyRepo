-- here I copy filels from every folder in 'root'
-- to 'root' with special name adjustings

import System.Directory
import Control.Monad

mapi f list = mapi' 1 f list
    where
        mapi' i f []     = []
        mapi' i f (x:xs) = f i x : mapi' (i + 1) f xs 

combinePaths []       = []
combinePaths (x:[])   = x
combinePaths (x:y:xs) = x ++ "\\" ++ combinePaths (y:xs)


copyWithRename root folder i x = 
    copyFile
        ( combinePaths [root, folder, x] )
        ( combinePaths [root, folder ++ "_" ++ show i ++ ".doc"])

        
processFolder root folder = do
    content <- listDirectory (combinePaths [root, folder])
    sequence $ mapi (copyWithRename root folder) content

main = 
    do
        content <- listDirectory root
        sequence $ map (processFolder root) content
    where
        root = "C:\\Temp"
    
