import Data.List
--todo : ignore case
--"ad" -> ^a.*d.*$ -> ^ad.*
--"*ad" -> a.*d 


data Line a = Line [a] a
data Matrix a = MatrixLayer a (Line a) (Line a) 


-- addCost    x = 1
-- deleteCost x = 1

addCost (Just _)  = 1
addCost Nothing   = 1

deleteCost (Just _)  = 1
deleteCost Nothing   = 1

--weightCell add delete substitute = min (min (add + addCost $ ) (delete + deleteCost)) substitute--


oneWeight xLetter yLetter b c a = min (min (a + (addCost xLetter)) (c + (deleteCost yLetter))) substitute
                                  where substitute = b + if xLetter == yLetter then 0 else 1

                                  
weightOneLine xLetter yLetter xLetters (Line xArray xBound) cross =
    let oneWeightLettered (xLetter, xShifted, x) = oneWeight yLetter xLetter xShifted x
        toWeights = map oneWeightLettered $ zip3 (xLetters ++ [xLetter]) (xBound : xArray) (xArray ++ [cross])
        line = scanl (flip ($)) (xBound + 1) toWeights
    in (Line (tail . reversedTail $ line) (head line), last line)

reversedTail [] = undefined
reversedTail xs = take ((length xs) - 1) xs

attachToLine (Line line bound) x = Line (line ++ [x]) bound 
                                  
--weight from to =
block (xLetters, yLetters, (MatrixLayer cross lineX lineY)) (xLetter, yLetter) =
    let (xLine, xCross)  = weightOneLine xLetter yLetter xLetters lineX cross
        (yLine, yCross)  = weightOneLine yLetter xLetter yLetters lineY cross
        cross' = oneWeight xLetter yLetter cross yCross xCross
    in (xLetters ++ [xLetter], yLetters ++ [yLetter], (MatrixLayer cross' (attachToLine xLine xCross) (attachToLine yLine yCross)))
        

longZip left right =
    let longZip' (x:xs) (y:ys) = (Just x,    Just y)  : longZip' xs ys
        longZip' []     (y:ys) = (Nothing,   Just y)  : longZip' [] ys
        longZip' (x:xs) []     = (Just x,    Nothing) : longZip' xs []
        longZip' []     []     = []
    in longZip' left right


    
do1 left right =
    let zipped = longZip left right
        (_, _,(MatrixLayer result _ _)) = foldl block ([Just ' '], [Just ' '], (MatrixLayer 0 (Line [] 1) (Line [] 1))) zipped
    in result
    
--test = block 'e' 'e' "ar" "dag" 3 3 [2,2] [3,3] 3

fromWord = "dagestan"
toWord = "arestant"

test1 = do1 fromWord toWord -- 8
test3 = do1 "" ""           -- 0


test4 list =
 let list' = do
              x <- list
              y <- list
              
              return (x, y, (do1 x y))
 in filter (\ (_, _, x) -> x /= 0 &&  x < 5 ) list' 
 
 
--main = test4 exampleList 
 
 
exampleList = [
               "dagestan",
               "arestant",
               "Gorod",
               "Zima",
               "Lazeyka",
               "Yazyik",
               "Oduvanchik",
               "Na postsovetskom",
               "Peyzazh",
               "Pervyiy skazochnik",
               "Ptitsyi",
               "Kochevnik",
               "Stolnyiy gorod",
               "Tolko vpered",
               "Ryitsar i drakon",
               "Utoplennik",
               "O kotah i kryisah",
               "Molchalivyie teni",
               "Osvobozhdenie Osventsima",
               "Na rubezhe",
               "Ukroschenie konya",
               "Chayka",
               "Poslyi",
               "Poema mednogo kolossa",
               "Kartonnyiy nemets",
               "Vesna",
               "Prospekt",
               "U severnogo morya",
               "Trolleybus",
               "Pesenka zhYoludya",
               "Razmyishleniya za chaem",
               "Ryinok",
               "Angel",
               "XXI vek",
               "Ptichiy zamok",
               "Za naslednika prestola",
               "Rzhavyie latyi",
               "Dekadentskaya muza",
               "Les",
               "Step",
               "Letnee",
               "Pustyinya",
               "Mashinnyiy kod",
               "Bezbiletnik",
               "Opyat osen",
               "Iggdrasil"]
    
