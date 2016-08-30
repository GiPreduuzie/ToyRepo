module Lib
    ( source
    , conduit'
    , sink'
    ) where


import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad

source :: Source Maybe Int
--source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4
    
-- conduit :: Conduit Int IO String
-- conduit = do
    -- -- Get all of the adjacent pairs from the stream
    -- mi1 <- await
    -- mi2 <- await
    -- case (mi1, mi2) of
        -- (Just i1, Just i2) -> do
            -- yield $ show (i1, i2)
            -- --leftover i2
            -- conduit
        -- _ -> return ()
            
-- sink :: Sink String IO ()
-- sink = do
    -- mstr <- await
    -- case mstr of
        -- Nothing -> return ()
        -- Just str -> do
            -- liftIO $ putStrLn str
            -- sink
            
conduit' :: Conduit Int Maybe String
conduit' = do value <- await
              case value of 
                  Nothing -> return ()
                  Just x  -> do 
                                yield $ show x
                                conduit'
                         
sink' = do 
         value <- await
         case value of 
             Nothing     -> return ()
             Just value  -> do lift $ Just value
                               sink'
            
-- sink' :: Sink String IO ()            
-- sink' = do
    -- value <- await
    -- case value of
        -- Nothing    -> return ()
        -- Just value -> liftIO $ putStrLn value
                      -- sink'
