-- module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc


{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Main

import           Yesod
import           Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.IORef
import           Control.Monad
import           XmlParser

data HelloWorld = HelloWorld
     { surveys :: IORef (Map.Map Integer String)
     , counter :: IORef Int
     }
     
mkYesod "HelloWorld" [parseRoutes|
/                 HomeR          GET
/samara           SamaraR        GET
/kostroma         KostromaR      GET
/damasl           DamaskR        GET
-- /astrakhan/#Text  AstrakhanR     GET
/astrakhan        AstrakhanR     GET POST 
-- /kazan            KazanR      GET
/kazan/count      KazanCountR    GET
/kazan/count/add  KazanAddCountR GET
/survey/#Integer  SurveyIdR      GET
|]



instance Yesod HelloWorld

--getHomeR :: Handler Html
getHomeR           = defaultLayout [whamlet|<a href=@{SamaraR}>Go to Samara <a href=@{KostromaR}>Go to Kostroma<a href=@{DamaskR}>Go to DamaskR|]
getSamaraR         = defaultLayout [whamlet|<a href=@{KostromaR}>Go to Kostroma|]
getKostromaR       = defaultLayout [whamlet|<a href=@{HomeR}>Go home|]
getDamaskR         = return $ object ["msg" .= "Hello World!"]
getKazanCountR     = do 
                        counterRef <- fmap counter getYesod
                        counter <- liftIO $ (modifyValue' counterRef)
                        return $ object ["count" .= show counter]
                        
getKazanAddCountR  = do 
                         counterRef <- fmap counter getYesod
                         counter <- liftIO $ (modifyValue  counterRef)
                         return $ object ["count" .= show counter]
                         
-- getKazanR          = do 
                        -- surveysRef <- fmap surveys getYesod
                        
                        -- surveys <- readIORef surveysRef
                        
                        -- join $ return $ object ["msg" .= (show  surveys)]
                         
--getAstrakhanR text = return $ object ["msg" .=  text]

getAstrakhanR =      do 
                        surveysRef <- fmap surveys getYesod
                        surveys <- liftIO $ (modifyValue' surveysRef)
                        return $ object ["count" .= show surveys]

postAstrakhanR = 
     do 
        (params, files) <- runRequestBody
        let params'  = (show ([ show name | (name, value) <- params]))
        let files'   = (show ([ show name | (name, value) <- files]))
        
        surveysRef <- fmap surveys getYesod
        surveys <- liftIO $ atomicModifyIORef surveysRef $ \x -> (updateMap x files', updateMap x files')
        
        return $ object ["params" .= show params', 
                         "files"  .= show files']
                         
getSurveyIdR id = 
       do 
        surveysRef <- fmap surveys getYesod
        surveys <- liftIO $ (modifyValue' surveysRef)
        
        let result = processSurvey (Map.lookup id surveys)
        
        return $ object ["value" .= show result]
                         
processSurvey x = processXml x                       

-- modifyValue  :: IORef Int -> IO Int
modifyValue  counterRef = atomicModifyIORef counterRef update
                          where update x = (x+1, x+1)
                         

modifyValue' counterRef = readIORef counterRef
                         
maximum' zero [] = zero
maximum' zero (x:xs) = maximum $ zero : x : xs

getNewKey x = (maximum' 0 (Set.toList (Map.keysSet x)))+1

updateMap map value = Map.insert (getNewKey map) (show value) map
                         
main :: IO ()
main = do 
           counterRef <- newIORef 0
           surveysRef <- newIORef (Map.fromList [(1, "The list")])
           warp 3000 HelloWorld
                 { surveys = surveysRef
                 , counter = counterRef 
                 }