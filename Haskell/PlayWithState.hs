{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad.State
import Diagrams.Prelude
import Diagrams.Backend.Pdf.CmdLine
--import Diagrams.Backend.SVG.CmdLine

harmonicStep :: State (Double, Double) Double
harmonicStep = do
    (position, velocity) <- get
    let acceleration = (-0.01 * position)
        velocity' = velocity + acceleration
        position' = position + velocity'
    put (position', velocity')
    return position
    
harmonic :: State (Double, Double) [Double]
harmonic = do
    position <- harmonicStep
    laterPosition <- harmonic
    return (position : laterPosition)
    
b1 = square 20 # lw 0.002

main = defaultMain (pad 1.1 b1)
    
--main = mainWith (circle 1 :: Diagram B)  --mapM print (take 100 . fst $ runState harmonic (1, 1))