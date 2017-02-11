{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Yesod
import           Yesod.Core.Types
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import           Language.Haskell.TH.Ppr

import           MatrixProcessor

data MappingServer = MappingServer
     
mkYesod "MappingServer" [parseRoutes|
/combinesurveys   CombineSurveysR   POST
|]

instance Yesod MappingServer
                      
postCombineSurveysR =
    do
      text <- result
      let matrix = foldl (++) "" text
      matrix' <- liftIO $ handleErrors (processMatrix matrix)
      
      return $ object ["matrix" .= matrix']
    where 
      result = rawRequestHandler rawRequestBody
      
      
handleErrors (Right x) = x
handleErrors (Left  x) = return $ handleErrors' x

handleErrors' MatrixCouldNotBeParsed          = "Matrix could not be parsed"
handleErrors' MasterIsNotDefined              = "Master survey is not defined"
handleErrors' MultipleMasterSurveyDefinition  = "Multiple defenition of master survey"


rawRequestHandler x = x $$ CL.map (bytesToString . BS.unpack) =$ CL.consume
      
                         
main :: IO ()
main = warp 3001 MappingServer