{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Job where

import HEP.Automation.Model.Client.Config

-- import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Types
-- import Data.Aeson.Encode
import Data.Aeson.Parser

import qualified Data.Attoparsec as A

import System.FilePath

type Url = String 

startCreate :: ModelConfiguration -> FilePath -> IO () 
startCreate _mc _fp = do 
  putStrLn "job started"

startGet :: ModelConfiguration -> String -> IO () 
startGet mc name = do 
  putStrLn $"get " ++ name
  let url = modelconf_modelserverurl mc 
  r <- getJsonFromServer url ("model" </> name) 
  putStrLn $ show r 


startPut :: ModelConfiguration -> FilePath -> IO () 
startPut _mc _fp = do 
  putStrLn "job started"

startDelete :: ModelConfiguration -> String -> IO () 
startDelete _mc _name = do 
  putStrLn "job started"


getJsonFromServer :: Url -> String -> IO (Either String (Result Value))
getJsonFromServer url api = do 
  requestget <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestgetjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "error" 
