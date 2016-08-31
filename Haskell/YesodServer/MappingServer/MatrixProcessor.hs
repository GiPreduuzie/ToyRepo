{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module MatrixProcessor
( 
  MyError(..)
, processMatrix
) where

import           Data.Aeson (decode, encode)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as BL
import           DatabaseClient

-- import           DatabaseClient (getSurvey)

 
data Survey = Survey { id :: String, isMaster :: Bool } 
             deriving (Show)
             
data Matrix = Matrix { name :: String, surveys :: [Survey] }
             deriving (Show)

$(deriveJSON defaultOptions ''Survey)             
$(deriveJSON defaultOptions ''Matrix)


data MyError =
      MatrixCouldNotBeParsed
    | MasterIsNotDefined
    | MultipleMasterSurveyDefinition
    
processMatrix :: String -> Either MyError (IO String)
processMatrix x = 
    case result of
         Just value -> Right value
         Nothing -> Left MatrixCouldNotBeParsed
    where
        result = 
         do 
             Matrix name surveys <- decode (BL.pack x)
             return $ processSurveys surveys
             
processSurveys :: [Survey] -> IO String
processSurveys surveys = 
    do 
        x <- ioSurveys
        return $ foldl (++) "" x
    where ioSurveys = sequence $ map processSurvey surveys
        
processSurvey (Survey id isMaster) = getSurvey id

             
-- "{\"name\":\"matrix name\", \"surveys\":[{\"id\":\"some id\",\"isMaster\":true},{\"id\":\"some id\",\"isMaster\":true}]}"