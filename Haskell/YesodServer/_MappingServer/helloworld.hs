{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
import           Yesod
import           Yesod.Core.Types
--import           Network.Wai
import           Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.IORef
import           Control.Monad
import           XmlParser
import           Network.Wai
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import           Control.Monad.Trans.Class
import           Language.Haskell.TH.Ppr

data HelloWorld = HelloWorld
     { 
       surveys :: IORef (Map.Map Integer String)
     , counter :: IORef Int
     }
     
mkYesod "HelloWorld" [parseRoutes|
/                 HomeR          GET
/samara           SamaraR        GET
/kostroma         KostromaR      GET
/damasl           DamaskR        GET
-- /astrakhan/#Text  AstrakhanR     GET
-- /astrakhan        AstrakhanR     GET POST 
-- /kazan            KazanR      GET
/kazan/count      KazanCountR    GET
/kazan/count/add  KazanAddCountR GET


/survey           SurveyR        POST
/survey/#Integer  SurveyIdR      GET  PUT
/surveys          SurveysR       GET
/count            CountR         GET


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
                         

-- getAstrakhanR =      do 
                        -- surveysRef <- fmap surveys getYesod
                        -- surveys <- liftIO $ (modifyValue' surveysRef)
                        -- return $ object ["count" .= show surveys]
                        
                        
                        

postSurveyR =
    do
      text' <- result
            
      let text = foldl (++) "" text'
      let updateMap' = (\ x -> updateMap x text)
      surveysRef <- fmap surveys getYesod
      
      surveys' <- liftIO $ (modifyValue' surveysRef)
      let key = getNewKey surveys'
      
      _ <- liftIO $ atomicModifyIORef surveysRef $ \x -> (updateMap' x, updateMap' x)
      
      return $ object ["id" .= key]
    where 
      result = rawRequestHandler rawRequestBody
   
getSurveyIdR id = 
       do 
        surveysRef <- fmap surveys getYesod
        surveys <- liftIO $ (modifyValue' surveysRef)
        
        counterRef <- fmap counter getYesod
        counter <- liftIO $ (modifyValue counterRef)
        
        let result = do survey <- (Map.lookup id surveys)
                        return $ survey
           
        return $ object ["survey" .= result]
        
putSurveyIdR id = 
    do
      text' <- result
            
      let text = foldl (++) "" text'
      let updateMap'' = (\ x -> updateMap' x id text)
      surveysRef <- fmap surveys getYesod
      
      _ <- liftIO $ atomicModifyIORef surveysRef $ \x -> (updateMap'' x, updateMap'' x)
      
      return ()
    where 
      result = rawRequestHandler rawRequestBody
      
      
getSurveysR = 
    do 
        surveysRef <- fmap surveys getYesod
        surveys <- liftIO $ (modifyValue' surveysRef)
        let keys = Set.toList . Map.keysSet $ surveys
           
        return $ object ["surveys" .= keys]
        
getCountR = 
    do 
        counterRef <- fmap counter getYesod
        counter <- liftIO $ (modifyValue'  counterRef)
        return $ object ["count" .= counter]
        
getFileContent x = "some"
                         
processSurvey x = (processXml x) ++ "\n" ++ x

-- modifyValue  :: IORef Int -> IO Int
modifyValue  counterRef = atomicModifyIORef counterRef update
                          where update x = (x+1, x+1)
                         

modifyValue' counterRef = readIORef counterRef
                         
maximum' zero [] = zero
maximum' zero (x:xs) = maximum $ zero : x : xs

getNewKey x = (maximum' 0 (Set.toList (Map.keysSet x)))+1

--updateMap :: Map.Map Integer String -> String -> (Integer, Map.Map Integer String)
updateMap map value = 
    map'
        where 
         key  = getNewKey map
         map' = Map.insert key value map
         
updateMap' map key value = Map.insert key value map
  
--rawRequestHandler :: MonadHandler m => Source m BS.ByteString -> m [String]
rawRequestHandler x = x $$ CL.map (bytesToString . BS.unpack) =$ CL.consume
                         
main :: IO ()
main = do 
           counterRef <- newIORef 0
           surveysRef <- newIORef (Map.fromList [(1, "The list")])
           warp 3001 HelloWorld
                 { surveys = surveysRef
                 , counter = counterRef 
                 }