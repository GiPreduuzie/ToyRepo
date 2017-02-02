import System.IO
import qualified Data.Text as Text

isProperLine line = 
    elem '8' (Text.unpack number)
    where number = (Text.split (\x -> x == '\t') $ line) !! 2


main = 
    do
        content <- readFile "/home/johanan/temp/poetlist"
        mapM putStrLn . map Text.unpack . filter isProperLine . Text.split (\ x -> x == '\n') . Text.pack $ content
