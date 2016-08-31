module DatabaseClient
( getSurvey
) where


-- import           Control.Monad.IO.Class  (liftIO)
import           Network.HTTP.Conduit    (Response (..), http, simpleHttp, withManager, parseUrl)
import           Language.Haskell.TH.Ppr (bytesToString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy as BS

getSurvey :: String -> IO String
getSurvey surveyId = 
       do 
           value <- simpleHttp ("http://localhost:3000/survey/" ++ surveyId)
           return $ bytesToString . BS.unpack $ (value)
       
                  
    -- withManager $ \manager -> do
        -- req <- liftIO . parseUrl $ ("http://localhost:3000/survey/" ++ surveyId)
        -- res <- http req manager
        -- result <- responseBody res $$ CL.map (bytesToString . BS.unpack) =$ CL.consume
        -- return result